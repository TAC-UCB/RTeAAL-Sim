package essent

import java.io.{File, FileWriter, Writer}
import ujson._

import essent.Emitter._
import essent.Extract._
import essent.ir._
import essent.Util._
import essent.TeAALDFGraphExtractor._
import firrtl._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.TransformManager.TransformDependency
import firrtl.stage.transforms

import logger._

import collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}


class EssentEmitter(initialOpt: OptFlags, w: Writer, outputDir: String, topName: String, circuit: Circuit) extends LazyLogging {

  val flagVarName = "PARTflags"
  implicit val rn: Renamer = new Renamer
  val actTrac = new ActivityTracker(w, initialOpt)
  val vcd: Option[Vcd] = if (initialOpt.withVCD) Some(new Vcd(circuit,initialOpt,w,rn)) else None

  // Declaring Modules
  //----------------------------------------------------------------------------
  def declareModule(m: Module, topName: String): Unit = {
    val registers = findInstancesOf[DefRegister](m.body)
    val memories = findInstancesOf[DefMemory](m.body)
    val registerDecs = registers flatMap {d: DefRegister => {
      val typeStr = genCppType(d.tpe)
      val regName = d.name
      Seq(s"$typeStr $regName;")
    }}

    val memDecs = memories map {m: DefMemory => {
      // if m.dpeth is not power of 2, then pad it to next power of 2
      if ((m.depth & (m.depth - 1)) != 0) {
        val depthInt = m.depth.toInt 
        val m_array_len = 1 << (32 - Integer.numberOfLeadingZeros(depthInt - 1))
        s"${genCppType(m.dataType)} ${m.name}[${m_array_len}];"
      } else {
        s"${genCppType(m.dataType)} ${m.name}[${m.depth}];"
      }
    }}

    val modulesAndPrefixes = findModuleInstances(m.body)
    val moduleDecs = modulesAndPrefixes map { case (module, fullName) => {
      val instanceName = fullName.split("\\.").last
      s"$module $instanceName;"
    }}
    val modName = m.name
    w.writeLines(0, "")
    w.writeLines(0, s"typedef struct $modName {")
    w.writeLines(1, registerDecs)
    w.writeLines(1, memDecs)
    w.writeLines(1, m.ports flatMap emitPort(modName == topName))
    w.writeLines(1, moduleDecs)
    w.writeLines(0, "")
    w.writeLines(1, s"$modName() {")
    w.writeLines(2, initializeVals(modName == topName)(m, registers, memories))
    w.writeLines(1, "}")
    if (modName == topName) {
      w.writeLines(0, "")
      // w.writeLines(1, s"void connect_harness(CommWrapper<struct $modName> *comm);")
    } else {
      w.writeLines(0, s"} $modName;")
    }
  }

  def declareExtModule(m: ExtModule): Unit = {
    val modName = m.name
    w.writeLines(0, "")
    w.writeLines(0, s"typedef struct $modName {")
    w.writeLines(1, m.ports flatMap emitPort(true))
    w.writeLines(0, s"} $modName;")
  }


  // Write General-purpose Eval
  //----------------------------------------------------------------------------
  // TODO: move specialized CondMux emitter elsewhere?
  def writeBodyInner(indentLevel: Int, sg: StatementGraph, opt: OptFlags,
                     keepAvail: Set[String] = Set()): Unit = {
    sg.stmtsOrdered() foreach { stmt => stmt match {
      case cm: CondMux => {
        if (rn.nameToMeta(cm.name).decType == MuxOut)
          w.writeLines(indentLevel, s"${genCppType(cm.mux.tpe)} ${rn.emit(cm.name)};")
        // println("what is e: ", cm.mux.cond)
        val muxCondRaw = emitExpr(cm.mux.cond)
        val muxCond = if (muxCondRaw == "reset") s"UNLIKELY($muxCondRaw)" else muxCondRaw
        w.writeLines(indentLevel, s"if (UNLIKELY($muxCond)) {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.tWay), opt)
        w.writeLines(indentLevel, "} else {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.fWay), opt)
        w.writeLines(indentLevel, "}")
      }
      case _ => {
        // println("stmt: ", stmt)
        w.writeLines(indentLevel, emitStmt(stmt))
        if (opt.withVCD)  vcd.get.compSmallEval(stmt, indentLevel)
        if (opt.trackSigs) actTrac.emitSigTracker(stmt, indentLevel)
      }
    }}
  }

  def checkRegResetSafety(sg: StatementGraph): Unit = {
    val updatesWithResets = sg.allRegDefs() filter { r => emitExpr(r.reset) != "UInt<1>(0x0)" }
    assert(updatesWithResets.isEmpty)
  }


  // Write Zoning Optimized Eval
  //----------------------------------------------------------------------------
  def genEvalFuncName(partID: Int): String = "EVAL_" + partID

  def genDepPartTriggers(consumerIDs: Seq[Int], condition: String): Seq[String] = {
    consumerIDs.sorted map { consumerID => s"$flagVarName[$consumerID] |= $condition;" }
  }

  def genAllTriggers(signalNames: Seq[String], outputConsumers: Map[String, Seq[Int]],
      suffix: String): Seq[String] = {
    selectFromMap(signalNames, outputConsumers).toSeq flatMap { case (name, consumerIDs) => {
      genDepPartTriggers(consumerIDs, s"${rn.emit(name)} != ${rn.emit(name + suffix)}")
    }}
  }

  def writeZoningPredecs(
                          sg: StatementGraph,
                          condPartWorker: MakeCondPart,
                          topName: String,
                          extIOtypes: Map[String, Type],
                          opt: OptFlags): Unit = {
    // predeclare part outputs
    val outputPairs = condPartWorker.getPartOutputsToDeclare()
    val outputConsumers = condPartWorker.getPartInputMap()
    w.writeLines(1, outputPairs map {case (name, tpe) => s"${genCppType(tpe)} ${rn.emit(name)};"})
    val extIOCacheDecs = condPartWorker.getExternalPartInputTypes(extIOtypes) map {
      case (name, tpe) => s"${genCppType(tpe)} ${rn.emit(name + condPartWorker.cacheSuffix)};"
    }
    w.writeLines(1, extIOCacheDecs)
    w.writeLines(1, s"std::array<bool,${condPartWorker.getNumParts()}> $flagVarName;")
    // FUTURE: worry about namespace collisions with user variables
    w.writeLines(1, s"bool sim_cached = false;")
    w.writeLines(1, s"bool regs_set = false;")
    w.writeLines(1, s"bool update_registers;")
    w.writeLines(1, s"bool done_reset;")
    w.writeLines(1, s"bool verbose;")
    w.writeLines(0, "")
    sg.stmtsOrdered() foreach { stmt => stmt match {
      case cp: CondPart => {
        w.writeLines(1, s"void ${genEvalFuncName(cp.id)}() {")
        if (!cp.alwaysActive)
          w.writeLines(2, s"$flagVarName[${cp.id}] = false;")
        if (opt.trackParts)
          w.writeLines(2, s"${actTrac.actVarName}[${cp.id}]++;")

        val cacheOldOutputs = cp.outputsToDeclare.toSeq map {
          case (name, tpe) => { s"${genCppType(tpe)} ${rn.emit(name + condPartWorker.cacheSuffix)} = ${rn.emit(name)};"
        }}
        w.writeLines(2, cacheOldOutputs)
        val (regUpdates, noRegUpdates) = partitionByType[RegUpdate](cp.memberStmts)
        val keepAvail = (cp.outputsToDeclare map { _._1 }).toSet
        val bodySG = StatementGraph(noRegUpdates)
        if (opt.conditionalMuxes)
          MakeCondMux(bodySG, rn, keepAvail)
        writeBodyInner(2, bodySG, opt, keepAvail)
        w.writeLines(2, genAllTriggers(cp.outputsToDeclare.keys.toSeq, outputConsumers, condPartWorker.cacheSuffix))
        val regUpdateNamesInPart = regUpdates flatMap findResultName
        w.writeLines(2, genAllTriggers(regUpdateNamesInPart, outputConsumers, "$next"))
        // triggers for MemWrites
        val memWritesInPart = cp.memberStmts collect { case mw: MemWrite => mw }
        val memWriteTriggers = memWritesInPart flatMap { mw => {
          // println("what is e: ", mw.wrEn)
          val condition = s"${emitExprWrap(mw.wrEn)} && ${emitExprWrap(mw.wrMask)}"
          genDepPartTriggers(outputConsumers.getOrElse(mw.memName, Seq()), condition)
        }}
        w.writeLines(2, memWriteTriggers)
        w.writeLines(2, regUpdates flatMap emitStmt)

        w.writeLines(1, "}")
      }
      case _ => throw new Exception(s"Statement at top-level is not a CondPart (${stmt.serialize})")
    }}
    w.writeLines(0, "")
  }

  def writeZoningBody(sg: StatementGraph, condPartWorker: MakeCondPart, opt: OptFlags): Unit = {
    w.writeLines(2, "if (reset || !done_reset) {")
    w.writeLines(3, "sim_cached = false;")
    w.writeLines(3, "regs_set = false;")
    w.writeLines(2, "}")
    w.writeLines(2, "if (!sim_cached) {")
    w.writeLines(3, s"$flagVarName.fill(true);")
    w.writeLines(2, "}")
    w.writeLines(2, "sim_cached = regs_set;")
    w.writeLines(2, "this->update_registers = update_registers;")
    w.writeLines(2, "this->done_reset = done_reset;")
    w.writeLines(2, "this->verbose = verbose;")
    val outputConsumers = condPartWorker.getPartInputMap()
    val externalPartInputNames = condPartWorker.getExternalPartInputNames()
    // do activity detection on other inputs (external IOs and resets)
    w.writeLines(2, genAllTriggers(externalPartInputNames, outputConsumers, condPartWorker.cacheSuffix))
    // cache old versions
    val extIOCaches = externalPartInputNames map {
      sigName => s"${rn.emit(sigName + condPartWorker.cacheSuffix)} = ${rn.emit(sigName)};"
    }
    w.writeLines(2, extIOCaches.toSeq)
    sg.stmtsOrdered() foreach { stmt => stmt match {
      case cp: CondPart => {
        if (!cp.alwaysActive)
          w.writeLines(2, s"if (UNLIKELY($flagVarName[${cp.id}])) ${genEvalFuncName(cp.id)}();")
        else
          w.writeLines(2, s"${genEvalFuncName(cp.id)}();")
      }
      case _ => w.writeLines(2, emitStmt(stmt))
    }}
    // w.writeLines(2,  "#ifdef ALL_ON")
    // w.writeLines(2, s"$flagVarName.fill(true);" )
    // w.writeLines(2,  "#endif")
    w.writeLines(2, "regs_set = true;")
  }


  // General Structure (and Compiler Boilerplate)
  //----------------------------------------------------------------------------
  def execute(circuit: Circuit): Unit = {
    val opt = initialOpt
    val topName = circuit.main
    val headerGuardName = topName.toUpperCase + "_H_"
    w.writeLines(0, s"#ifndef $headerGuardName")
    w.writeLines(0, s"#define $headerGuardName")
    w.writeLines(0, "")
    w.writeLines(0, "#include <array>")
    w.writeLines(0, "#include <cstdint>")
    w.writeLines(0, "#include <cstdlib>")
    w.writeLines(0, "#include \"uint.h\"")
    w.writeLines(0, "#include \"sint.h\"")
    w.writeLines(0, "#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)")
    if (opt.trackParts || opt.trackSigs || opt.withVCD) {
      w.writeLines(0, "#include <fstream>")
    }

    if(opt.withVCD) {
      w.writeLines(0, "uint64_t vcd_cycle_count = 0;")
      w.writeLines(1,s"""FILE *outfile;""")
      // TODO: size this buffer based on widest signal present in the design
      w.writeLines(1,s"""char VCD_BUF[2000];""")
    }
    val sg = StatementGraph(circuit, opt.removeFlatConnects)

    logger.info(sg.makeStatsString())
    val containsAsserts = sg.containsStmtOfType[Stop]()
    val extIOMap = findExternalPorts(circuit)
    val condPartWorker = MakeCondPart(sg, rn, extIOMap)
    rn.populateFromSG(sg, extIOMap)
    if (opt.useCondParts) {
      condPartWorker.doOpt(opt.partCutoff)
    } else {
      if (opt.regUpdates)
        OptElideRegUpdates(sg)
      if (opt.conditionalMuxes)
        MakeCondMux(sg, rn, Set())
    }
    checkRegResetSafety(sg)
    if (opt.trackParts || opt.trackSigs || opt.trackExts)
      actTrac.declareTop(sg, topName, condPartWorker)

    // [RTeAALSim] Add Intermediate Variable structure
    val designfileName = initialOpt.firInputFile.toPath.getFileName.toString
    var designName = ""
    if (designfileName == "freechips.rocketchip.system.DefaultConfig.fir") {
      println("[INFO] Rocketchip Default Design.")
      designName = "rocketchip-1c"
    } else if (designfileName == "freechips.rocketchip.system.DualCoreConfig.fir") {
      println("[INFO] Rocketchip 2 Core")
      designName = "rocketchip-2c"
    } else if (designfileName == "freechips.rocketchip.system.QuadCoreConfig.fir") {
      println("[INFO] Rocketchip 4 Core")
      designName = "rocketchip-4c"
    } else if (designfileName == "freechips.rocketchip.system.HexaCoreConfig.fir") {
      println("[INFO] Rocketchip 6 Core")
      designName = "rocketchip-6c"
    } else if (designfileName == "freechips.rocketchip.system.OctaCoreConfig.fir") {
      println("[INFO] Rocketchip 8 Core")
      designName = "rocketchip-8c"
    } else if (designfileName == "freechips.rocketchip.system.TwelveConfig.fir") {
      println("[INFO] Rocketchip 12 Core")
      designName = "rocketchip-12c"
    } else if (designfileName == "freechips.rocketchip.system.SixteenConfig.fir") {
      println("[INFO] Rocketchip 16 Core")
      designName = "rocketchip-16c"
    } else if (designfileName == "freechips.rocketchip.system.TwentyConfig.fir") {
      println("[INFO] Rocketchip 20 Core")
      designName = "rocketchip-20c"
    } else if (designfileName == "freechips.rocketchip.system.SmallBoomConfig.fir") {
      println("[INFO] SmallBoom Design.")
      designName = "smallboom-1c"
    } else if (designfileName == "freechips.rocketchip.system.DualSmallBoomConfig.fir") {
      println("[INFO] SmallBoom 2 Core Design.")
      designName = "smallboom-2c"
    } else if (designfileName == "freechips.rocketchip.system.QuadSmallBoomConfig.fir") {
      println("[INFO] SmallBoom 4 Core Design.")
      designName = "smallboom-4c"
    } else if (designfileName == "freechips.rocketchip.system.HexaSmallBoomConfig.fir") {
      println("[INFO] SmallBoom 6 Core Design.")
      designName = "smallboom-6c"
    } else if (designfileName == "freechips.rocketchip.system.OctaSmallBoomConfig.fir") {
      println("[INFO] SmallBoom 8 Core Design.")
      designName = "smallboom-8c"
    } else if (designfileName == "freechips.rocketchip.system.TwelveSmallBoomConfig.fir") {
      println("[INFO] SmallBoom 12 Core Design.")
      designName = "smallboom-12c"
    } else if (designfileName == "freechips.rocketchip.system.MediumBoomConfig.fir") {
      println("[INFO] MediumBoom Design.")
      designName = "mediumboom-1c"
    } else if (designfileName == "freechips.rocketchip.system.LargeBoomConfig.fir") {
      println("[INFO] LargeBoom Design.")
      designName = "largeboom-1c"
    } else if (designfileName == "freechips.rocketchip.system.DualLargeBoomConfig.fir") {
      println("[INFO] LargeBoom 2 Core Design.")
      designName = "largeboom-2c"
    } else if (designfileName == "freechips.rocketchip.system.QuadLargeBoomConfig.fir") {
      println("[INFO] LargeBoom 4 Core Design.")
      designName = "largeboom-4c"
    } else if (designfileName == "freechips.rocketchip.system.HexaLargeBoomConfig.fir") {
      println("[INFO] LargeBoom 6 Core Design.")
      designName = "largeboom-6c"
    } else if (designfileName == "freechips.rocketchip.system.OctaLargeBoomConfig.fir") {
      println("[INFO] LargeBoom 8 Core Design.")
      designName = "largeboom-8c"
    } else if (designfileName == "freechips.rocketchip.system.MegaBoomConfig.fir") {
      println("[INFO] MegaBoom Design.")
      designName = "megaboom-1c"
    } else if (designfileName == "freechips.rocketchip.system.DualMegaBoomConfig.fir") {
      println("[INFO] MegaBoom 2 Core Design.")
      designName = "megaboom-2c"
    } else if (designfileName == "freechips.rocketchip.system.QuadMegaBoomConfig.fir") {
      println("[INFO] MegaBoom 4 Core Design.")
      designName = "megaboom-4c"
    } else if (designfileName == "freechips.rocketchip.system.HexaMegaBoomConfig.fir") {
      println("[INFO] MegaBoom 6 Core Design.")
      designName = "megaboom-6c"
    } else if (designfileName == "freechips.rocketchip.system.OctaMegaBoomConfig.fir") {
      println("[INFO] MegaBoom 8 Core Design.")
      designName = "megaboom-8c"
    } else if (designfileName == "freechips.rocketchip.system.RocketGemminiConfig.fir") {
      println("[INFO] Rocketchip + Gemmini Design.")
      designName = "gemmini-16"
    } else if (designfileName == "freechips.rocketchip.system.LargeRocketGemminiConfig.fir") {
      println("[INFO] Rocketchip + Large Gemmini Design.")
      designName = "gemmini-32"
    } else if (designfileName == "freechips.rocketchip.system.RocketSHA3Config.fir") {
      println("[INFO] Rocketchip + SHA3 1 stage Design.")
      designName = "sha3-1"
    } else if (designfileName == "freechips.rocketchip.system.RocketMedianSHA3Config.fir") {
      println("[INFO] Rocketchip + SHA3 2 stage Design.")
      designName = "sha3-2"
    } else if (designfileName == "freechips.rocketchip.system.RocketLargeSHA3Config.fir") {
      println("[INFO] Rocketchip + SHA3 4 stage Design.")
      designName = "sha3-4"
    } else if (designfileName == "freechips.rocketchip.system.SmallRocketGemminiConfig.fir") {
      println("[INFO] Rocketchip + Small Gemmini Design.")
      designName = "gemmini-8"
    } else {
      assert(false, s"Unknown design: ${designfileName}")
    }

    circuit.modules foreach {
      case m: Module => declareModule(m, topName)
      case m: ExtModule => declareExtModule(m)
    }
    val topModule = findModule(topName, circuit) match {case m: Module => m}
    if (initialOpt.writeHarness) {
      w.writeLines(0, "")
      w.writeLines(1, s"void connect_harness(CommWrapper<struct $topName> *comm) {")
      w.writeLines(2, HarnessGenerator.harnessConnections(topModule))
      w.writeLines(1, "}")
      w.writeLines(0, "")
    }
    if (opt.withVCD)  { vcd.get.declareOldvaluesAll(circuit) }
    if(opt.withVCD) { vcd.get.genWaveHeader() }
    if (containsAsserts) {
      w.writeLines(1, "bool assert_triggered = false;")
      w.writeLines(1, "int assert_exit_code;")
      w.writeLines(0, "")
    }
    if (opt.useCondParts)
      writeZoningPredecs(sg, condPartWorker, circuit.main, extIOMap, opt)
    w.writeLines(1, s"void eval(bool update_registers, bool verbose, bool done_reset) {")
    if(opt.withVCD) { vcd.get.initializeOldValues(circuit) }
    if (opt.trackParts || opt.trackSigs)
      w.writeLines(2, "act_cycle_count++;")
    if (opt.useCondParts)
      writeZoningBody(sg, condPartWorker, opt)
    else
      writeBodyInner(2, sg, opt)
    if(opt.withVCD) { vcd.get.compareOldValues(circuit) }
    if (containsAsserts) {
      w.writeLines(2, "if (done_reset && update_registers && assert_triggered) exit(assert_exit_code);")
      w.writeLines(2, "if (!done_reset) assert_triggered = false;")
    }
    w.writeLines(0, "")
    if(opt.withVCD) { vcd.get.assignOldValues(circuit) }
    w.writeLines(2, "")
    w.writeLines(1, "}")
    w.writeLines(0, "")
    w.writeLines(0, "")
    w.writeLines(0, s"} $topName;") //closing top module dec
    w.writeLines(0, "")
    w.writeLines(0, s"#endif  // $headerGuardName")
    w.close()


    // =========================== TeAAL DFGraph Extraction =========================
    val ju = new TeAALUtil()
    val startTime = System.nanoTime()
    ju.memoryIdxRead(s"accum_length_dict_${designName}.json")
    ju.memoryLengthRead(s"mem_array_len_${designName}.json")

    // graph extraction
    val te = new TeAALExtractor(ju.memory_array)

    sg.stmtsOrdered() foreach ( stmt =>
        te.extractStmt(stmt)
    )
    val mem_extra_inputs = te.mem_reg_update()
    te.largeOpBreakDown.clear()
    te.inOutMap.clear()

    // const prop
    TeAALDFGraphConstProp.update_op(te.op_counter)
    val tconstp = new TeAALDFGraphConstProp(te.opInBufferOrder, te.inEdgeBuffer, te.outEdgeBuffer, te.oEBufferR, 
                  te.bitWidthMetadata)
    tconstp.tconstp_all_func()

    val dfGraph = new TeAALDFGraphExtractor(te.opInBufferOrder, te.inEdgeBuffer, te.outEdgeBuffer, te.oEBufferR, 
                  te.bitWidthMetadata, te.regOrder, te.regNextOrder, tconstp.muxJumpTable, te.dshiftGroup, mem_extra_inputs, tconstp.randomChain_Inputs)
    dfGraph.DFGraphGen()

    // Report printing
    val treport = new TeAALReport(tconstp.op_counter, tconstp.uniqueOps, dfGraph.LAYER_NUM, dfGraph.maxL, dfGraph.TOTAL_IOP, 
                  te.inEdgeBuffer, te.outEdgeBuffer, dfGraph.inNeigh, dfGraph.outNeigh)
    treport.DFGraph_Report()

    te.inEdgeBuffer.clear()
    te.outEdgeBuffer.clear()

    dfGraph.inNeigh.clear()
    dfGraph.outNeigh.clear()
    dfGraph.iEBufferR.clear()

    val teAALTranspiler = new TeAALTranspiler(dfGraph.opIndex)
    teAALTranspiler.dtmSignal(dfGraph.dtmSignalCollect, "../../kernel/json", s"dtmSignal_${designName}")
    // teAALTranspiler.debugSignal(dfGraph.debugMap, "../../kernel/json", s"debugSignal_${designName}")
    teAALTranspiler.InputRegMap(dfGraph.input_idx_map, "../../kernel/json", s"InputRegMap_${designName}")
    teAALTranspiler.extractDim(dfGraph.nodeIDOrderMap)
    teAALTranspiler.jumpTableGen(tconstp.muxJumpTable, dfGraph.layerNodeIDN, "../../kernel/txt", s"muxJT_${designName}")
    teAALTranspiler.tensorTocircInYML(dfGraph.circuitInput, "../../kernel/txt", s"layerIn_${designName}")
    teAALTranspiler.tensorToInYML(dfGraph.nodeIDOrderMap, dfGraph.LAYER_NUM, dfGraph.maxL, te.ORank, "I", "S", "O", "../../kernel/yaml", s"operationInputMask_${designName}_maskedIn_full_func")
    teAALTranspiler.tensorToOpYML(dfGraph.isnMap, dfGraph.LAYER_NUM, dfGraph.maxL, "I", "S", "../../kernel/yaml", s"operationMask_${designName}_maskedIn_full_func")
    teAALTranspiler.kernels_gen(dfGraph.isnMap, dfGraph.nodeIDOrderMap, dfGraph.circuitInput, "../../kernel", s"${designName}")
    
    val endTime = System.nanoTime()
    val duration = (endTime - startTime) / 1000000000

    println(s"Execution time: $duration seconds")

    // =========================== ======================== =========================



  }
}


class EssentCompiler(opt: OptFlags) {

  // VerilogMemDelays: compiling memory latencies to combinational-read memories with delay pipelines.
  // This pass eliminates mems with read latency = 1 (introduced by CHIRRTL smem)
  // and thus satisfy essent.pass.FactorMemReads (memHasRightParams)

  // ConvertAsserts: Convert Verification IR (with op == Formal.Assert) into conventional print statement

  val readyForEssent: Seq[TransformDependency] =
    Seq(
      Dependency(firrtl.passes.memlib.VerilogMemDelays),
      Dependency(essent.passes.RemoveFormalNCover),
      Dependency(firrtl.transforms.formal.ConvertAsserts)
    ) ++
    firrtl.stage.Forms.LowFormOptimized ++
    Seq(
//      Dependency(essent.passes.LegacyInvalidNodesForConds),
      Dependency(essent.passes.ReplaceAsyncRegs),
      Dependency(essent.passes.NoClockConnects),
      Dependency(essent.passes.RegFromMem1),
      Dependency(essent.passes.FactorMemReads),
      Dependency(essent.passes.FactorMemWrites),
      Dependency(essent.passes.SplitRegUpdates),
      Dependency(essent.passes.FixMulResultWidth),
      Dependency(essent.passes.DistinctTypeInstNames),
      Dependency(essent.passes.RemoveAsAsyncReset),
      Dependency(essent.passes.ReplaceRsvdKeywords)
    )

  def compileAndEmit(circuit: Circuit): Unit = {
    val topName = circuit.main
    if (opt.writeHarness) {
      val harnessFilename = new File(opt.outputDir(), s"$topName-harness.cc")
      val harnessWriter = new FileWriter(harnessFilename)
      if (opt.withVCD) { HarnessGenerator.topFile(topName, harnessWriter," |  dut.genWaveHeader();") }
      else { HarnessGenerator.topFile(topName, harnessWriter, "")}
      harnessWriter.close()
    }
    val firrtlCompiler = new transforms.Compiler(readyForEssent)
    val resultState = firrtlCompiler.execute(CircuitState(circuit, Seq()))
    if (opt.dumpLoFirrtl) {
      val debugFilename = new File(opt.outputDir(), s"$topName.lo.fir")
      val debugWriter = new FileWriter(debugFilename)
      debugWriter.write(resultState.circuit.serialize)
      debugWriter.close()
    }

    val outputDir = "../../utils/bin"
    val dutFile = new File(outputDir, s"$topName.h")
    val dutWriter = new FileWriter(dutFile)

    val emitter = new EssentEmitter(opt, dutWriter, outputDir, topName, resultState.circuit)
    emitter.execute(resultState.circuit)
    dutWriter.close()
  }
}
