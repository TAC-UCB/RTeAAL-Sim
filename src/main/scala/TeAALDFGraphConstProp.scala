package essent

import essent.ir._
import essent.TeAALUtil._
import essent.TeAALExtractor._
import firrtl._
import firrtl.ir._
import firrtl.PrimOps._
import collection.mutable.{ArrayBuffer, HashMap, HashSet}
import scala.io.Source

class TeAALDFGraphConstProp(opInBufferOrder: opInOrder, inEdgeBuffer: BufferHashMap, outEdgeBuffer: BufferHashMap, oEBufferR: HashMap[String, String], 
                  bitWidthMetadata: HashMap[String, Int]) {

    import TeAALDFGraphConstProp._
    import TeAALExtractor.{BufferHashMap, opInOrder}

    val ju = new TeAALUtil()

    var uniqueOps = 0
    var constPropagation = HashMap[String, String]()
    var op_counter = op_c

    // mux chain record
    var muxRecord = HashMap[Int, HashMap[String, String]]() // mux name, {condname: condname, tval: val, fval: val}
    var muxCondRecord = HashMap[Int, HashMap[String, HashMap[String, HashSet[String]]]]() // mux name, {condname: vals involved in the condname op}
    var muxChainForm = HashMap[String, HashMap[Int, HashMap[String, String]]]()
    var muxJumpTable = ArrayBuffer[String]()
    var muxChainCollect = HashSet[ArrayBuffer[Int]]()
    var muxChainInfo = HashMap[ArrayBuffer[Int], ArrayBuffer[String]]()

    var randomChain_Inputs = HashMap[String, Int]() 
    var bitChain_order = HashMap[String, Boolean]() // true descending, false ascending

    val SimDTM_output: HashSet[String] =
      HashSet.from(
        Source.fromFile("preserved_signals.txt")
          .getLines()
          .map(_.trim)
          .filter(_.nonEmpty)
      )

    def opNameParse(operation: String): String = {
      operation.split("_\\+_")(0)
    }


    def constPropRecord(inName: String, outName: String): Unit = {
      if (ju.isInte(inName)) {
        constPropagation += outName->inName
      } else if (constPropagation.contains(inName)) {
        constPropagation += outName->inName
      }
    }


    def constExtract(): Unit = {
      inEdgeBuffer foreach {case(in, ops)=>{
        ops foreach {op=>{
          val output = if (oEBufferR.contains(op)) oEBufferR(op) else ""
          if (!SimDTM_output.contains(output)) {
            if (op.contains("assign")) {
              constPropRecord(in, oEBufferR(op))
            }
          }
        }}
      }}
    }

    def allInputNum(inputs: HashMap[String, HashSet[Int]]): Boolean = {
      inputs foreach {case(input, indices)=>{
        if (!(ju.isInte(input) || ju.isDecimalInt(input))) {
          return false
        }
      }} 
      return true
    }

    def opCompute(operation: String, opTpe: String, inputs: HashMap[String, HashSet[Int]], output: String) = {
      opTpe match {
        case "add" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 2))
          val result = ju.formatAsUIntPattern(firstInput + secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "sub" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 2))
          val result = ju.formatAsUIntPattern(firstInput - secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "mul" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 2))
          val result = ju.formatAsUIntPattern(firstInput * secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "mulS" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 2))
          val result = ju.formatAsSIntPattern(firstInput * secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "div" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 2))
          val result = ju.formatAsUIntPattern(firstInput / secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "divS" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 2))
          val result = ju.formatAsSIntPattern(firstInput / secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "rem" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 2))
          val result = ju.formatAsUIntPattern(firstInput % secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "lt" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(if (firstInput < secondInput) BigInt(1) else BigInt(0), 1)
          bitWidthMetadata(result) = 1
          constPropRecord(result, output)
        }
        case "ltS" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(if (firstInput < secondInput) BigInt(1) else BigInt(0), 1)
          bitWidthMetadata(result) = 1
          constPropRecord(result, output)
        }
        case "leq" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(if (firstInput <= secondInput) BigInt(1) else BigInt(0), 1)
          bitWidthMetadata(result) = 1
          constPropRecord(result, output)
        }
        case "leqS" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(if (firstInput <= secondInput) BigInt(1) else BigInt(0), 1)
          bitWidthMetadata(result) = 1
          constPropRecord(result, output)
        }
        case "eq" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(if (firstInput == secondInput) BigInt(1) else BigInt(0), 1)
          bitWidthMetadata(result) = 1
          constPropRecord(result, output)
          if (operation == "eq_+_16860") {
            println(s"Const prop eq op: $operation with inputs $firstInput and $secondInput gives result $result")
          }
        }
        case "neq" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(if (firstInput != secondInput) BigInt(1) else BigInt(0), 1)
          bitWidthMetadata(result) = 1
          constPropRecord(result, output)
        }
        case "and" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(firstInput & secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "or" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(firstInput | secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "xor" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val result = ju.formatAsUIntPattern(firstInput ^ secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "not" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val bitWidth = bitWidthMetadata(operation)
          val mask = (BigInt(1) << bitWidth) - 1  // 0xFF
          val result = ju.formatAsUIntPattern(~firstInput & mask, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "cat" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1))
          val sec_bw = ju.inputSearch(inputs, 2).toInt
          val concatenated = (firstInput << sec_bw) | secondInput
          val result = ju.formatAsUIntPattern(concatenated, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "xorr" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val result = ju.formatAsUIntPattern(firstInput.toString(2).count(_ == '1') % 2, 1)
          bitWidthMetadata(result) = 1
          constPropRecord(result, output)
        }
        case "bits" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.inputSearch(inputs, 1).toInt
          val thirdInput = ju.inputSearch(inputs, 2).toInt

          val bite = (firstInput >> secondInput) & (1 << (secondInput - thirdInput + 1) - 1)
          val result = ju.formatAsUIntPattern(bite, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "shl" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1)).toInt
          val result = ju.formatAsUIntPattern(firstInput << secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "shr" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1)).toInt
          val result = ju.formatAsUIntPattern(firstInput >> secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "shrS" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1)).toInt
          val result = ju.formatAsUIntPattern(firstInput >> secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "dshl" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1)).toInt
          val result = ju.formatAsUIntPattern(firstInput << secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "dshr" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1)).toInt
          val result = ju.formatAsUIntPattern(firstInput >> secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "dshrS" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.stringInt(ju.inputSearch(inputs, 1)).toInt
          val result = ju.formatAsUIntPattern(firstInput >> secondInput, bitWidthMetadata(operation))
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
        case "assign" => {
          constPropRecord(ju.inputSearch(inputs, 0), output)
        }
        case "asSInt" => {
          val firstInput = ju.stringInt(ju.inputSearch(inputs, 0))
          val secondInput = ju.inputSearch(inputs, 1).toInt
          val extend_bit = (firstInput >> secondInput) & 1
          val result = if (extend_bit == 1) {
            val extend_mask = ((BigInt(1) << 64)-1) - ((BigInt(1) << (secondInput+1)) - 1)
            ju.formatAsSIntPattern(firstInput | extend_mask, bitWidthMetadata(operation))
          } else {
            ju.formatAsSIntPattern(firstInput, bitWidthMetadata(operation))
          }
          bitWidthMetadata(result) = bitWidthMetadata(operation)
          constPropRecord(result, output)
        }
      }
    }

    def constTrans(operation: String): Boolean = {
      val inputs = opInBufferOrder(operation)
      val opTpe = opNameParse(operation)
      val output = if (opTpe == "stop" || opTpe == "memw" || opTpe == "memr") "" else oEBufferR(operation)
      var updateIn = false

      opTpe match {
        case "stop" | "memw" | "memr" => {}
        case "mux" => {
          val condInput = ju.inputSearch(inputs, 0)
          if (ju.isInte(condInput)) {
            updateIn = true
            val new_op = operation.replace(opTpe, "assign")

            bitWidthMetadata += new_op->bitWidthMetadata(operation)
            bitWidthMetadata -= operation

            if (ju.hexInt(condInput).toInt == 1) {
              val tval = ju.inputSearch(inputs, 1)
              opInBufferOrder += new_op->HashMap(tval->HashSet(0))
              inEdgeBuffer.getOrElseUpdate(tval, HashSet.empty) += new_op
              constPropRecord(tval, output)
            } else {
              val fval = ju.inputSearch(inputs, 2)
              opInBufferOrder += new_op->HashMap(fval->HashSet(0))
              inEdgeBuffer.getOrElseUpdate(fval, HashSet.empty) += new_op
              constPropRecord(fval, output)
            }
            oEBufferR(new_op) = output
            oEBufferR.remove(operation)
            outEdgeBuffer(oEBufferR(new_op)) += new_op
            outEdgeBuffer(oEBufferR(new_op)) -= operation
            opInBufferOrder(operation) foreach {case(ins, indices)=>{
              inEdgeBuffer(ins) -= operation
              if (inEdgeBuffer(ins).isEmpty) {
                inEdgeBuffer.remove(ins)
              }
            }}
            opInBufferOrder.remove(operation)
          }
        }
        case _  => {
          if (allInputNum(inputs)) {
            updateIn = true
            opCompute(operation, opTpe, inputs, output)
            opInBufferOrder.remove(operation)
            oEBufferR.remove(operation)
            inputs foreach {case(in, idx)=>{
              inEdgeBuffer(in) -= operation
              if (inEdgeBuffer(in).isEmpty) {
                inEdgeBuffer.remove(in)
              }
            }}
            outEdgeBuffer.remove(output)
            bitWidthMetadata -= operation
            bitWidthMetadata -= output
          }
        }
      }
      updateIn
    }

    def constProp_rec(): Unit = {
      val outEdgeBuffer_cp = new BufferHashMap()
      outEdgeBuffer foreach (kv => outEdgeBuffer_cp += kv._1->kv._2.clone())
      var update = true
      while (update) {  
        update = false
        outEdgeBuffer_cp foreach {case(output, operation)=>{
          if (!SimDTM_output.contains(output)) {
            val op = operation.head
            if (constPropagation.contains(output)) {
              val constIn = constPropagation(output)
              if (inEdgeBuffer.contains(output)) {
                inEdgeBuffer.getOrElseUpdate(constIn, HashSet.empty) ++= inEdgeBuffer(output).clone()
                inEdgeBuffer(constIn) -= op

                bitWidthMetadata -= op
                bitWidthMetadata -= output

                inEdgeBuffer.remove(output)
                outEdgeBuffer.remove(output)
                oEBufferR.remove(op)
                opInBufferOrder.remove(op)
                inEdgeBuffer(constIn) foreach (opupdate=>{
                  opInBufferOrder(opupdate) foreach{case(in, idx)=>{
                    if (in == output) {
                      opInBufferOrder(opupdate).getOrElseUpdate(constIn, HashSet.empty) ++= opInBufferOrder(opupdate)(output).clone()
                      opInBufferOrder(opupdate).remove(output)
                    }
                  }}
                })
                update = true

                var updateIn = true 
                // while (updateIn) {
                  inEdgeBuffer(constIn) foreach (op=>{
                    // println("start trans: ", ops, opInBufferOrder(ops))
                    if ((op != "stop") && (op != "memw") && (op != "memr"))
                      updateIn = constTrans(op)
                  })
                // }
              }
            }
          }
        }}
      }
    }

    def deadRegElim(): HashSet[String] = {
      val rvmOps = HashSet[String]()
      outEdgeBuffer foreach {case(output, ops)=>{
        if (!SimDTM_output.contains(output) && !output.contains("@next")&& !output.contains("_fake_output") && !output.contains("io_success")) {
          if (!inEdgeBuffer.contains(output)) {
            // println(s"Dead register/elimination found at output: $output, ${ops.mkString(", ")}")
            ops foreach {op=>{
              if (!op.contains("chain") && !op.contains("memw") && !op.contains("dsh"))
                rvmOps += op
            }}
          }
        }}
      }

      rvmOps foreach {op=>{
        val inputs = opInBufferOrder(op)
        opInBufferOrder.remove(op)
        val outName = oEBufferR(op)
        oEBufferR.remove(op)
        outEdgeBuffer.remove(outName)
        
        val toRemove = scala.collection.mutable.ArrayBuffer[String]()
        inputs foreach {case(in, idxs)=>{
          inEdgeBuffer.getOrElseUpdate(in, HashSet.empty) -= op
          if (inEdgeBuffer(in).isEmpty) {
            toRemove += in
          }
        }}
        toRemove.foreach(inEdgeBuffer.remove)
      }}
      rvmOps
    }

    def deadRegElimFixpoint(): Unit = {
      var changed = true
      var iter = 0

      while (changed) {
        iter += 1
        val removed = deadRegElim()
        println(s"[DCE] iteration $iter removed ${removed.size} ops")
        changed = removed.nonEmpty
      }
    }


    def nonUsedRegElim(): Unit = {
      val rvmOps = HashSet[String]()
      outEdgeBuffer foreach {case(output, ops)=>{
        if (output.contains("@next")) {
          val baseName = output.split("@next")(0)
          if (inEdgeBuffer.contains(baseName)) {
            // println(s"Dead register/elimination found at output: $output, ${ops.mkString(", ")}")
            val inops = inEdgeBuffer(baseName)
            inops foreach {inop=>{
              if (inop.contains("assign")) {
                if (oEBufferR(inop) == output) {
                  rvmOps += inop
                }
              }
            }}
          }
        }}
      }

      rvmOps foreach {op=>{
        val inputs = opInBufferOrder(op)
        opInBufferOrder.remove(op)
        // check inputs size -- key is 1, values is 1
        if (inputs.size != 1 || inputs.head._2.size != 1) {
          throw new Exception(s"Unexpected inputs size for dead reg elim op $op: ${inputs}")
        }
        val outName = oEBufferR(op)
        if (!outName.contains("@next")) {
          throw new Exception(s"Unexpected output name for dead reg elim op $op: $outName")
        }
        oEBufferR.remove(op)
        outEdgeBuffer.remove(outName)
        
        val toRemove = scala.collection.mutable.ArrayBuffer[String]()
        inputs foreach {case(in, idxs)=>{
          inEdgeBuffer.getOrElseUpdate(in, HashSet.empty) -= op
          if (inEdgeBuffer(in).isEmpty) {
            toRemove += in
          }
        }}
        toRemove.foreach(inEdgeBuffer.remove)
      }}
    }

    def caseChainChecker(root: String, chain: HashSet[String]): Boolean = {
      // check:
      // 1. all muxes in the chain use eq as selector, and the variable being compared is the same across all eq ops
      // 2. check that the fval of each mux is only connected to muxes in the chain (except for the root mux)
      // return true if both conditions are met

      // Helper: check if the selector op is eq and with one var and one int as inputs
      def eqChecker(mux_op: String): Boolean = {
        val ins = opInBufferOrder(mux_op)
        val condName = ju.inputSearch(ins, 0)
        val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
        preOps.headOption match {
          case Some(eqOp) =>
            val opType = eqOp.split("_\\+_")(0)
            if (opType != "eq") return false
            val eqIns = opInBufferOrder(eqOp)
            val in1 = ju.inputSearch(eqIns, 0)
            val in2 = ju.inputSearch(eqIns, 1)
            return (ju.isInte(in1) && !ju.isInte(in2)) || (!ju.isInte(in1) && ju.isInte(in2))
          case None => false
        }
      }

      // Helper: find condition variable of a mux
      def condVar(mux_op: String): Option[String] = {
        val ins = opInBufferOrder(mux_op)
        val condName = ju.inputSearch(ins, 0)
        val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
        preOps.headOption.flatMap { eqOp =>
          val eqIns = opInBufferOrder(eqOp)
          val in1 = ju.inputSearch(eqIns, 0)
          val in2 = ju.inputSearch(eqIns, 1)
          if (ju.isInte(in1) && !ju.isInte(in2)) Some(in2)
          else if (!ju.isInte(in1) && ju.isInte(in2)) Some(in1)
          else None
        }
      }

      def topoSort(root: String, chain: HashSet[String]): Boolean = {
        val visited = HashSet[String]()
        val stack = ArrayBuffer[String]()

        def dfs(op: String): Unit = {
          if (!visited.contains(op) && chain.contains(op)) {
            visited += op
            val ins = opInBufferOrder(op)
            val fvalName = ju.inputSearch(ins, 2)
            val preOps = outEdgeBuffer.getOrElse(fvalName, HashSet.empty)

            preOps.foreach(dfs)
            stack += op
          }
        }

        dfs(root)
        if (stack.size != chain.size) {
          return false
        }
        true
      }

      if (!chain.forall(eqChecker)) {
        return false
      }
      // check all muxes have the same condition variable
      val condVars = chain.flatMap(condVar)
      if (condVars.size != 1) {
        return false
      }
      if (!topoSort(root, chain)) {
        return false
      }
      // true

      // check all mux except for the chain's output is only used by muxes in the chain
      chain forall { op => {
        val outName = oEBufferR(op)
        val nextOps = inEdgeBuffer.getOrElse(outName, HashSet.empty)
        ((nextOps.size == 1) && chain.contains(nextOps.head)) || (op == root)
      }}
    }

    def randomChainCheck(root: String, chain: HashSet[String]): Boolean = {
      // check:
      // 1. each mux only has 1 output and that output is passed to another mux's tval or fval in the chain (except for the root mux)
      chain forall { op => {
        val outName = oEBufferR(op)
        val nextOps = inEdgeBuffer.getOrElse(outName, HashSet.empty)
        if (op == root) {
          true
        } else {
          if (nextOps.size != 1) {
            false
          } else {
            val nextOp = nextOps.head
            if (!chain.contains(nextOp)) {
              false
            } else {
              val nextIns = opInBufferOrder(nextOp)
              val tvalName = ju.inputSearch(nextIns, 1)
              val fvalName = ju.inputSearch(nextIns, 2)
              (outName == tvalName) || (outName == fvalName)
            }
          }
        }
      }}
    }

    def findMuxChainRoot(): HashSet[String] = {
      var ret = HashSet[String]()
      var mux_count = 0
      opInBufferOrder foreach {case(op, ins)=>{
          if (op.contains("mux")) {
            mux_count += 1
            val condName = ju.inputSearch(ins, 0)
            val tvalName = ju.inputSearch(ins, 1)
            val fvalName = ju.inputSearch(ins, 2)
            val outName = oEBufferR(op)

            if (inEdgeBuffer.contains(outName)) {
              val muxBadCounts = inEdgeBuffer(outName).toSeq.collect {
                case inop if inop.startsWith("mux") => {
                  val idxs = ju.inputIdxSearch(opInBufferOrder(inop), outName)
                  (idxs.contains(1) || idxs.contains(2), idxs.contains(0))
                }
              }
              val countBad = muxBadCounts.count(_._1)
              val allOk = muxBadCounts.forall { case (hasBad, has0) => !hasBad || has0 }
              val allMuxAndIdxOk = allOk || countBad > 1

              if (allMuxAndIdxOk) {
                ret += op
              }
            }
            else {
              ret += op
            }
          }
        }
      }
      println(s"Total mux count: $mux_count")
      ret
    }

    def chainTraceBack(roots: HashSet[String]): HashMap[String, HashSet[String]] = {
      def dfs(op: String, visited: HashSet[String]): HashSet[String] = {
        if (!op.contains("mux") || visited.contains(op))
          return visited

        val chain = visited.union(HashSet(op))
        val ins = opInBufferOrder(op)
        val tvalName = ju.inputSearch(ins, 1)
        val fvalName = ju.inputSearch(ins, 2)

        // find producers of tval/fval
        val preOps = outEdgeBuffer.getOrElse(tvalName, HashSet.empty) ++
                    outEdgeBuffer.getOrElse(fvalName, HashSet.empty)

        preOps.foldLeft(chain) { (acc, pre) =>
          val opType = pre.split("_\\+_")(0)
          if (opType == "mux") {
            val preOut = oEBufferR(pre)                 // output signal of this mux
            val consumers = inEdgeBuffer.getOrElse(preOut, HashSet.empty)
            if (consumers.size == 1 && consumers.contains(op))
              dfs(pre, acc)
            else
              acc
          } else acc
        }
      }

      // collect (root -> chain) pairs and filter out small chains
      HashMap.from(
        roots.map { root =>
          val chain = dfs(root, HashSet.empty)
          root -> chain
        }.filter { case (_, chain) => chain.size > 2 }
      )
    }

    def caseStmtRetrieve(root: String, chain: HashSet[String]): Int = {
      val root_ins = opInBufferOrder(root)
      val condName = ju.inputSearch(root_ins, 0)

      // --- Step 1: extract selector variable from the root condition ---
      val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
      if (preOps.isEmpty || !preOps.forall(_.startsWith("eq"))) return 0

      val root_eq = preOps.head
      val eq_ins = opInBufferOrder(root_eq)
      val eq_in1 = ju.inputSearch(eq_ins, 0)
      val eq_in2 = ju.inputSearch(eq_ins, 1)

      // must be (variable, integer) or (integer, variable)
      if ((ju.isInte(eq_in1) && ju.isInte(eq_in2)) ||
          (!ju.isInte(eq_in1) && !ju.isInte(eq_in2))) {
        return 0
      }

      val selectorVar =
        if (ju.isInte(eq_in1)) eq_in2 else eq_in1

      // --- Step 2: check that every mux in the chain uses eq(selectorVar, <int>) ---
      val sameCond = chain.forall { op =>
        val ins = opInBufferOrder(op)
        val condName = ju.inputSearch(ins, 0)
        val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)

        preOps.nonEmpty && preOps.forall(_.contains("eq")) && preOps.exists { eqOp =>
          val eq_ins = opInBufferOrder(eqOp)
          val e1 = ju.inputSearch(eq_ins, 0)
          val e2 = ju.inputSearch(eq_ins, 1)
          // must involve selectorVar on one side, integer on the other
          (e1 == selectorVar && ju.isInte(e2)) ||
          (e2 == selectorVar && ju.isInte(e1))
        }
      }

      def topoSort(root: String, chain: HashSet[String]): Seq[String] = {
        val visited = HashSet[String]()
        val stack = ArrayBuffer[String]()

        def dfs(op: String): Unit = {
          if (!visited.contains(op) && chain.contains(op)) {
            visited += op
            val ins = opInBufferOrder(op)
            val fvalName = ju.inputSearch(ins, 2)
            val preOps = outEdgeBuffer.getOrElse(fvalName, HashSet.empty)

            preOps.foreach(dfs)
            stack += op
          }
        }

        dfs(root)
        stack.toSeq
      }

      val muxList = chain.toList
      val topo_chain = topoSort(root, chain)
      if (topo_chain.size != chain.size) {
        return 0
      }
      val head_mux = topo_chain.head
      // println(s"  chain root $root head mux $head_mux")
      val consistentChain = muxList.forall { op => {
        val ins = opInBufferOrder(op)
        val fvalName = ju.inputSearch(ins, 2)
        val preOps = outEdgeBuffer.getOrElse(fvalName, HashSet.empty)
        preOps.isEmpty || preOps.forall(chain.contains) || (op == head_mux)
      }}

      if (sameCond && consistentChain) 1 else 0
    }

    def chainClassification(chain_list: HashMap[String, HashSet[String]]): HashMap[Int, HashMap[String, HashSet[String]]] = {
      val ret = HashMap[Int, HashMap[String, HashSet[String]]]()
      chain_list.foreach { case (root, chain) =>
        val isCaseStmt = caseStmtRetrieve(root, chain)
        val inner = ret.getOrElseUpdate(isCaseStmt, HashMap[String, HashSet[String]]())
        inner(root) = chain   // mutate inner map directly
      }
      ret
    }

    def chainSplit(root: String, chain: HashSet[String]): HashMap[String, HashSet[String]] = {
      val ret = HashMap[String, HashSet[String]]()

      // Helper: check if the selector op is eq and with one var and one int as inputs
      def eqChecker(mux_op: String): Boolean = {
        val ins = opInBufferOrder(mux_op)
        val condName = ju.inputSearch(ins, 0)
        val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
        preOps.headOption match {
          case Some(eqOp) =>
            val opType = eqOp.split("_\\+_")(0)
            if (opType != "eq") return false
            val eqIns = opInBufferOrder(eqOp)
            val in1 = ju.inputSearch(eqIns, 0)
            val in2 = ju.inputSearch(eqIns, 1)
            return (ju.isInte(in1) && !ju.isInte(in2)) || (!ju.isInte(in1) && ju.isInte(in2))
          case None => false
        }
      }

      // Helper: find condition variable of a mux
      def condVar(mux_op: String): Option[String] = {
        val ins = opInBufferOrder(mux_op)
        val condName = ju.inputSearch(ins, 0)
        val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
        preOps.headOption.flatMap { eqOp =>
          val eqIns = opInBufferOrder(eqOp)
          val in1 = ju.inputSearch(eqIns, 0)
          val in2 = ju.inputSearch(eqIns, 1)
          if (ju.isInte(in1) && !ju.isInte(in2)) Some(in2)
          else if (!ju.isInte(in1) && ju.isInte(in2)) Some(in1)
          else None
        }
      }

      // Helper: topo sort the chain
      def topoSort(root: String, chain: HashSet[String]): Seq[String] = {
        val visited = HashSet[String]()
        val stack = ArrayBuffer[String]()

        def dfs(op: String): Unit = {
          if (!visited.contains(op) && chain.contains(op)) {
            visited += op
            val ins = opInBufferOrder(op)
            val tvalName = ju.inputSearch(ins, 1)
            val fvalName = ju.inputSearch(ins, 2)

            val preOps = outEdgeBuffer.getOrElse(tvalName, HashSet.empty) ++
                          outEdgeBuffer.getOrElse(fvalName, HashSet.empty)

            preOps.foreach(dfs)
            stack += op
          }
        }

        dfs(root)
        if (stack.size != chain.size) {
          throw new Exception(s"Topological sort failed: visited ${stack.size}, chain size ${chain.size}")
        }
        stack.reverse.toSeq
      }

      // Order chain so we process downstream first (root first)
      val orderedMuxes = topoSort(root, chain)
      var currentRoot: String = orderedMuxes.head
      var currentChain = HashSet[String](currentRoot)
      // var lastVar: Option[String] = None
      var lastMuxIsEq = eqChecker(currentRoot)
      var lastVar = if (lastMuxIsEq) condVar(currentRoot).get else None
      var lastfvalName = ju.inputSearch(opInBufferOrder(currentRoot), 2)

      for (op <- orderedMuxes.tail) {
        val isEq = eqChecker(op)
        val thisVar = if (isEq) condVar(op).get else None
        val outName = oEBufferR(op)

        val needSplit =
          (isEq != lastMuxIsEq) ||
          (lastVar != thisVar) || ((lastfvalName != outName) && isEq && lastMuxIsEq && (lastVar == thisVar))

        if (needSplit) {
          ret(currentRoot) = currentChain
          currentRoot = op
          currentChain = HashSet(op)
          lastVar = thisVar
          lastMuxIsEq = isEq
          lastfvalName = ju.inputSearch(opInBufferOrder(op), 2)
        } else {
          currentChain += op
          lastVar = thisVar
          lastMuxIsEq = isEq
          lastfvalName = ju.inputSearch(opInBufferOrder(op), 2)
        }
      }

      // store the last chain
      if (currentChain.nonEmpty)
        ret(currentRoot) = currentChain

      ret
    }

    def zeroCaseCoalesce(chain_list: HashMap[String, HashSet[String]]): HashMap[String, HashSet[String]] = {
      val ret = HashMap[String, HashSet[String]]()
      val visited = HashSet[String]()
      val chains = HashMap[String, HashSet[String]]()

      chain_list foreach {case (r, s)=>{
        val cs = caseStmtRetrieve(r, s)
        if (cs == 0) {
          chains(r) = s
        } else {
          ret(r) = s
        }
      }}

      val allNodes = chains.flatMap { case (r, s) => s }.toSet

      // find all connected components in allNodes
      allNodes foreach {node=>{
        if (!visited.contains(node)) {
          val mergedChain = HashSet[String]()
          val queue = ArrayBuffer[String](node)
          visited += node

          while (queue.nonEmpty) {
            val current = queue.remove(0)
            mergedChain += current

            // find neighbors in chains
            val outName = oEBufferR(current)
            val nextOp = inEdgeBuffer.getOrElse(outName, HashSet.empty)
            val tvalName = ju.inputSearch(opInBufferOrder(current), 1)
            val fvalName = ju.inputSearch(opInBufferOrder(current), 2)
            val prevOps = outEdgeBuffer.getOrElse(tvalName, HashSet.empty) ++
                           outEdgeBuffer.getOrElse(fvalName, HashSet.empty)

            val neighbors = HashSet[String]()
            nextOp.foreach { n =>
              if (allNodes.contains(n)) {
                neighbors += n
              }
            }
            prevOps.foreach { n =>
              if (allNodes.contains(n)) {
                neighbors += n
              }
            }

            neighbors.foreach { n =>
              if (!visited.contains(n)) {
                visited += n
                queue += n
              }
            }
          }

          // Find the true topmost root — not present as a child in any chain
          val trueRoot = mergedChain
            .map(_.split("_\\+_")(1).toInt)   // extract numeric part
            .maxOption                         // safely get max if list not empty
            .map(n => s"mux_+_$n")             // rebuild string
            .getOrElse("")                     // handle empty case

          ret(trueRoot) = mergedChain
        }
      }}

      ret
    }

    def classAdjust(classed_chains: HashMap[Int, HashMap[String, HashSet[String]]]): HashMap[Int, HashMap[String, HashSet[String]]] = {
      val ret = HashMap[Int, HashMap[String, HashSet[String]]]()
      val case0 = classed_chains.getOrElse(0, HashMap.empty)
      val case1 = classed_chains.getOrElse(1, HashMap.empty)

      case1 foreach { case (root, chain) => {
        val inner = ret.getOrElseUpdate(1, HashMap[String, HashSet[String]]())
        inner(root) = chain

        // println(s"  all one chain rooted at $root with size ${chain.size} classified as case 0")
        // println(s"  all one chain members: ${chain.mkString(", ")}")
      }}

      case0 foreach { case (root, chain) => {
        // println(s"Splitting chain rooted at $root with size ${chain.size}")
        // println(s"Chain members: ${chain.mkString(", ")}")

        val subChains = chainSplit(root, chain)
        val allZero = subChains.forall { case (newRoot, subChain) =>
          val cs = caseStmtRetrieve(newRoot, subChain)
          (cs == 0) || (cs == 1 && subChain.size <= 2)
        }

        if (allZero) {
          // no change, copy over
          val inner = ret.getOrElseUpdate(0, HashMap[String, HashSet[String]]())
          inner(root) = chain
          // println(s"  all zero chain rooted at $root with size ${chain.size} classified as case 0")
          // println(s"  all zero chain members: ${chain.mkString(", ")}")
        } else {
          val coalesced_subchains = zeroCaseCoalesce(subChains)
          coalesced_subchains foreach { case (newRoot, subChain) => {
            if (subChain.size > 2) {
              val isCaseStmt = caseStmtRetrieve(newRoot, subChain)
              val inner = ret.getOrElseUpdate(isCaseStmt, HashMap[String, HashSet[String]]())
              inner(newRoot) = subChain
            }
          }}
        }
      }}
      ret
    }

    def muxRecordReform(caseChains: HashMap[String, HashSet[String]]): (HashMap[Int, HashMap[String, String]], HashMap[Int, HashMap[String, HashMap[String, HashSet[String]]]]) = {
      var new_muxRecord = HashMap[Int, HashMap[String, String]]()
      var new_muxCondRecord = HashMap[Int, HashMap[String, HashMap[String, HashSet[String]]]]()

      caseChains foreach {case(root, chain)=>{
        chain foreach {op=>{
          val ins = opInBufferOrder(op)
          val condName = ju.inputSearch(ins, 0)
          val tvalName = ju.inputSearch(ins, 1)
          val fvalName = ju.inputSearch(ins, 2)
          val outName = oEBufferR(op)

          val record = HashMap[String, String]()
          record("condName") = condName
          record("tvalName") = tvalName
          record("fvalName") = fvalName
          record("outName") = outName
          val opCount = op.split("_\\+_")(1).toInt
          new_muxRecord(opCount) = record

          new_muxCondRecord += opCount->HashMap[String, HashMap[String, HashSet[String]]]()
          val inputs = HashMap[String, HashSet[String]]()
          opInBufferOrder(outEdgeBuffer(condName).head).keys foreach {input=>{
            if (ju.isInte(input)) {
                inputs.getOrElseUpdate("Number", HashSet.empty) += input
            } else {
                if (outEdgeBuffer.contains(input)) {
                    if(outEdgeBuffer(input).head.split("_\\+_")(0) == "assign") {
                        if (ju.isInte(opInBufferOrder(outEdgeBuffer(input).head).head._1)) {
                            inputs.getOrElseUpdate("Number", HashSet.empty) += opInBufferOrder(outEdgeBuffer(input).head).head._1
                        } else {
                            inputs.getOrElseUpdate("Var", HashSet.empty) += input
                        }
                    } else {
                        inputs.getOrElseUpdate("Var", HashSet.empty) += input
                    }
                } else {
                    inputs.getOrElseUpdate("Var", HashSet.empty) += input
                }
            }
          }}
          if (inputs.contains("Number") && inputs.contains("Var")) {
            new_muxCondRecord(opCount) += condName->inputs
          }

        }}
      }}
      (new_muxRecord, new_muxCondRecord)
    }

    def newMuxChainInfoExtract(caseChains: HashMap[String, HashSet[String]], new_muxRecord: HashMap[Int, HashMap[String, String]], new_muxCondRecord: HashMap[Int, HashMap[String, HashMap[String, HashSet[String]]]]): HashMap[ArrayBuffer[Int], ArrayBuffer[String]] = {
      val muxChainInfo = HashMap[ArrayBuffer[Int], ArrayBuffer[String]]()
      caseChains foreach { case(root, muxchain)=>{
        val chain = ArrayBuffer.from(muxchain.map(_.stripPrefix("mux_+_").toInt).toSeq.sorted)
        if (chain.nonEmpty) {
          val eg_op = chain.head
          val cond = new_muxRecord(eg_op)("condName")
          val bitwidth = ju.IntBWExtract(new_muxCondRecord(eg_op)(cond)("Number").head)
          if ((math.pow(2, bitwidth).toInt) / chain.size < 4) {
            val default_val = new_muxRecord(eg_op)("fvalName")
            var jump_array = ArrayBuffer.fill(math.pow(2, bitwidth).toInt)(default_val)
            chain foreach {op=>{
              val tval = new_muxRecord(op)("tvalName")  
              val cond = new_muxRecord(op)("condName")
              val jump_idx = ju.hexInt(new_muxCondRecord(op)(cond)("Number").head).toInt
              jump_array(jump_idx) = tval
            }}
            muxChainInfo(chain) = jump_array
          }
        }
      }}
      muxChainInfo
    }

    def new_rvmEqCond(new_muxChainInfo: HashMap[ArrayBuffer[Int], ArrayBuffer[String]], new_muxRecord: HashMap[Int, HashMap[String, String]]): Unit = {
      val realOpcount = ArrayBuffer(new_muxChainInfo.keys.flatten.toSeq: _*)
      new_muxRecord foreach {case(opCount, mux)=>{
        if (realOpcount.contains(opCount)) {
          val condName = mux("condName")
          if (outEdgeBuffer.contains(condName)) {
            if (inEdgeBuffer(condName).size == 1) {
              val eqOp = outEdgeBuffer(condName).head
              val eqInputs = opInBufferOrder(eqOp).keys
              eqInputs foreach {eqInput=>{
                inEdgeBuffer(eqInput) -= eqOp
                if (inEdgeBuffer(eqInput).isEmpty) {inEdgeBuffer.remove(eqInput)}
              }}
              oEBufferR.remove(eqOp)
              outEdgeBuffer.remove(condName)
              opInBufferOrder.remove(eqOp)
            }
          }
        }
      }}
    }

    def new_updateMuxChain(new_muxChainInfo: HashMap[ArrayBuffer[Int], ArrayBuffer[String]], new_muxRecord: HashMap[Int, HashMap[String, String]],  new_muxCondRecord: HashMap[Int, HashMap[String, HashMap[String, HashSet[String]]]]): Unit = {
      // rvm eq
      new_rvmEqCond(new_muxChainInfo, new_muxRecord)
      // rvm mux
      new_muxChainInfo foreach {case(chain, jump_array)=>{
        chain foreach {opCount =>{
          val operation = s"mux_+_${opCount}"
          val outName = new_muxRecord(opCount)("outName")
          val muxInputs = opInBufferOrder(operation).keys
          muxInputs foreach {muxInput=>{ 
            inEdgeBuffer(muxInput) -= operation
            if (inEdgeBuffer(muxInput).isEmpty) {inEdgeBuffer.remove(muxInput)}
          }}
          oEBufferR.remove(operation)
          outEdgeBuffer.remove(outName)
          opInBufferOrder.remove(operation)
        }}
      }}
      // add chain
      var jump_idx = 0
      new_muxChainInfo foreach {case(chain, jump_array)=>{
        val new_op = s"chain_+_${op_counter}"
        op_counter = op_counter + 1
        val outName = new_muxRecord(chain.max)("outName")
        val condName = new_muxRecord(chain.max)("condName")
        val condVar = new_muxCondRecord(chain.max)(condName)("Var").head
        addCondMux(outName, new_op, condVar, jump_idx.toString)

        var update_idx = 3
        jump_array foreach {jump_val=>{
          inEdgeBuffer.getOrElseUpdate(jump_val, HashSet.empty) += new_op
          opInBufferOrder(new_op).getOrElseUpdate(jump_val, HashSet.empty) += update_idx
          update_idx += 1
        }}

        jump_idx += jump_array.size
        muxJumpTable ++= jump_array
      }}
    }

    def breakBranchMux(root: String, chain: HashSet[String]): HashMap[String, HashSet[String]] = {
      val ret = HashMap[String, HashSet[String]]()
      val visitedGlobal = HashSet[String]()  // ensure each mux is only in one chain

      // Find all the "head" nodes = entry points of subchains
      val heads = HashSet[String]()
      chain.foreach { op =>
        val ins = opInBufferOrder(op)
        val tvalName = ju.inputSearch(ins, 1)
        val fvalName = ju.inputSearch(ins, 2)
        val preOps = outEdgeBuffer.getOrElse(tvalName, HashSet.empty) ++
                    outEdgeBuffer.getOrElse(fvalName, HashSet.empty)

        // A head has no predecessors inside this chain
        if (preOps.isEmpty || !preOps.exists(chain.contains)) {
          heads += op
        }
      }

      // DFS to collect a linear subchain starting from this head
      def dfs(op: String, chain: HashSet[String], localVisited: HashSet[String], stack: ArrayBuffer[String]): Unit = {
        if (!visitedGlobal.contains(op) && !localVisited.contains(op) && chain.contains(op)) {
          localVisited += op
          visitedGlobal += op

          val outName = oEBufferR(op)
          val nextOps = inEdgeBuffer.getOrElse(outName, HashSet.empty)
          // Continue along any downstream muxes
          if (nextOps.nonEmpty && nextOps.forall(chain.contains)) {
            nextOps.foreach(next => dfs(next, chain, localVisited, stack))
          }

          stack += op
        }
      }

      if (heads.size == 1) {
        return HashMap(root -> chain)
      }

      heads.foreach { head =>
        if (!visitedGlobal.contains(head)) {
          val localVisited = HashSet[String]()
          val stack = ArrayBuffer[String]()
          dfs(head, chain, localVisited, stack)
          val newRoot = stack.headOption.getOrElse(head)
          ret(newRoot) = HashSet.from(stack)
        }
      }
      ret
    }

    def stringChainStat(chains: HashMap[String, HashSet[String]]): HashMap[Int, HashMap[String, Int]] = {
      var ret = HashMap[Int, HashMap[String, Int]]()

      def topoSort(root: String, chain: HashSet[String]): Boolean = {
        val visited = HashSet[String]()
        val stack = ArrayBuffer[String]()

        def dfs(op: String): Unit = {
          if (!visited.contains(op) && chain.contains(op)) {
            visited += op
            val ins = opInBufferOrder(op)
            val tvalName = ju.inputSearch(ins, 1)
            val fvalName = ju.inputSearch(ins, 2)
            val preOps = outEdgeBuffer.getOrElse(fvalName, HashSet.empty) ++ 
                          outEdgeBuffer.getOrElse(tvalName, HashSet.empty)

            // if more than one predecessor in the chain, then it's not a string chain
            if (preOps.count(chain.contains) > 1) {
              return
            } else {
              preOps.foreach(dfs)
            }
            stack += op
          }
        }

        dfs(root)
        if (stack.size != chain.size) {
          return false
        }
        true
      }

      chains.foreach { case (root, chain) => {
        var inner_ret = ret.getOrElseUpdate(chain.size, HashMap[String, Int]())
        inner_ret.getOrElseUpdate("string chain", 0)
        inner_ret.getOrElseUpdate("branch mux", 0)
        val isDAG = topoSort(root, chain)
        if (!isDAG) {
          inner_ret("branch mux") += 1
        } else {
          inner_ret("string chain") += 1  
        }
      }}
      ret
    }

    def continuousBitCheck(chain: ArrayBuffer[String]): Boolean = {
      val varIn = HashSet[String]()
      val shiftBits = HashSet[Int]()
      chain.foreach { op => {
        val ins = opInBufferOrder(op)
        val condName = ju.inputSearch(ins, 0)
        val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
        val preOp = preOps.headOption.getOrElse("")
        if (preOp.nonEmpty) {
          val preOpIns = opInBufferOrder(preOp)
          val var_name = ju.inputSearch(preOpIns, 0)
          val shiftBit = ju.inputSearch(preOpIns, 1).toInt
          shiftBits += shiftBit
          val maskBit = ju.inputSearch(preOpIns, 2).toInt
          if (maskBit != 1)
            return false
          varIn += var_name
        } else {
          println(s"  continuousBitCheck: preOp is empty for mux $op")
          return false
        }
      }}
      if (varIn.size != 1) {
        println(s"  continuousBitCheck: multiple input vars found: ${varIn.mkString(", ")}")
        return false 
      }
      if (shiftBits.size != chain.size) {
        println(s"  continuousBitCheck: shiftBits size ${shiftBits.size} does not match chain size ${chain.size}")
        return false
      }
      // check if shiftBits are continuous
      val sortedBits = shiftBits.toSeq.sorted
      for (i <- 1 until sortedBits.size) {
        if (sortedBits(i) != sortedBits(i - 1) + 1) {
          println(s"  continuousBitCheck: non-continuous shift bits found: ${sortedBits.mkString(", ")}")
          return false
        }
      }
      true
    }

    def bitCondExtract(chains: HashMap[String, HashSet[String]]): HashMap[Int, HashMap[String, ArrayBuffer[String]]] = {
      val ret = HashMap[Int, HashMap[String, ArrayBuffer[String]]]() // 2 → bits, 0 → non-bits

      def topoSort(root: String, chain: HashSet[String]): Seq[String] = {
        val visited = HashSet[String]()
        val stack = ArrayBuffer[String]()

        def dfs(op: String): Unit = {
          if (!visited.contains(op) && chain.contains(op)) {
            visited += op
            val ins = opInBufferOrder(op)
            val tvalName = ju.inputSearch(ins, 1)
            val fvalName = ju.inputSearch(ins, 2)
            val preOps = outEdgeBuffer.getOrElse(tvalName, HashSet.empty) ++
                        outEdgeBuffer.getOrElse(fvalName, HashSet.empty)
            preOps.foreach(dfs)
            stack += op
          }
        }

        dfs(root)
        if (stack.size != chain.size) {
          throw new Exception(s"Topological sort failed: visited ${stack.size}, chain size ${chain.size}")
        }
        stack.reverse.toSeq // root first
      }

      chains.foreach { case (root, chain) =>
        val orderedChain = topoSort(root, chain)
        // val subchains = ArrayBuffer[HashSet[String]]()

        var currentSubChain = ArrayBuffer[String]()
        val subchains = ArrayBuffer[(Int, ArrayBuffer[String])]()
        var subRoot = root
        var lastVarIn = ""
        var currentType = 0 // 2 = bits, 0 = non-bits

        // def flushSubChain(): Unit = {
        //   if (currentSubChain.nonEmpty) {
        //     val inner = ret.getOrElseUpdate(currentType, HashMap[String, HashSet[String]]())
        //     inner(subRoot) = currentSubChain.clone()
        //     subchains += currentSubChain.clone()
        //     currentSubChain.clear()
        //   }
        // }

        def flushSubChain(): Unit = {
          if (currentSubChain.nonEmpty) {
            // subchains += currentSubChain.clone()
            subchains += ((currentType, currentSubChain.clone()))
            currentSubChain.clear()
          }
        }

        orderedChain.foreach { op =>
          val condName = ju.inputSearch(opInBufferOrder(op), 0)
          val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
          val preOp = preOps.headOption.getOrElse("")
          val opType = if (preOp.nonEmpty) preOp.split("_\\+_")(0) else ""
          val preOpIns = if (preOp.nonEmpty) opInBufferOrder(preOp) else HashMap[String, HashSet[Int]]()
          val varIn = if (preOp.nonEmpty) ju.inputSearch(preOpIns, 0) else ""

          val isBit = opType == "bits"
          val newType = if (isBit) 2 else 0

          val needSplit =
            newType != currentType ||
            (isBit && lastVarIn.nonEmpty && varIn != lastVarIn)

          if (needSplit) {
            flushSubChain()
            subRoot = op
          }

          currentType = newType
          lastVarIn = varIn
          currentSubChain += op
        }

        flushSubChain() // final flush

        // val allSmall = subchains.forall(_.size <= 2)
        val allSmall = subchains.forall { case (_, sub) => sub.size <= 2 }

        if (allSmall) {
          // merge back original chain under dominant type (most frequent)
          // val dominantType =
          //   subchains.groupBy(_._1).maxBy(_._2.size)._1 // type with most subchains
          val inner = ret.getOrElseUpdate(0, HashMap[String, ArrayBuffer[String]]())
          // inner(root) = chain.clone()
          inner(root) = ArrayBuffer.from(orderedChain)
        } else {
          // keep split subchains, each under its own type
          subchains.foreach { case (tpe, sub) =>
            if (sub.nonEmpty && sub.size > 2) {
              if (tpe == 2 && !continuousBitCheck(sub)) {
                // reclassify non-continuous bit subchain as type 0
                val inner = ret.getOrElseUpdate(0, HashMap[String, ArrayBuffer[String]]())
                inner(sub.head) = sub
              } else {
                val inner = ret.getOrElseUpdate(tpe, HashMap[String, ArrayBuffer[String]]())
                inner(sub.head) = sub
              }
            }
          }
        }

        // val totalSubSize = subchains.map(_.size).sum
        // val merged = subchains.flatten.toSet
        val totalSubSize = subchains.map(_._2.size).sum
        val merged = subchains.flatMap(_._2).toSet
        if (totalSubSize != chain.size || merged != chain) {
          val missing = chain.diff(merged)
          val extras = merged.diff(chain)
          println(s"[WARN] Chain $root mismatch:")
          println(s"  original size = ${chain.size}, subchain total = $totalSubSize")
          if (missing.nonEmpty) println(s"  missing ops: ${missing.mkString(", ")}")
          if (extras.nonEmpty) println(s"  duplicated ops: ${extras.mkString(", ")}")
        }
        // println(s"[INFO] Chain $root split into ${subchains.size} subchains.")
        // println(s"  subchains sizes: ${subchains.map(_._2.size).mkString(", ")}")
        // println("original chain: ", chain)
        // println("subchains: ", subchains)
      }

      ret
    }

    def splitNumber(n: Int, limit: Int = 8): ArrayBuffer[Int] = {
      if (n <= limit) {
        ArrayBuffer(n)
      } else {
        val a = n / 2
        val b = n - a
        splitNumber(a, limit) ++ splitNumber(b, limit)
      }
    }

    def splitLongChain(root: String, chain: ArrayBuffer[String], limit: Int = 10): HashMap[String, ArrayBuffer[String]] = {
      val ret = HashMap[String, ArrayBuffer[String]]()
      val chainSize = chain.size

      def topoSort(root: String, chain: ArrayBuffer[String]): Seq[String] = {
        val visited = HashSet[String]()
        val stack = ArrayBuffer[String]()

        def dfs(op: String): Unit = {
          if (!visited.contains(op) && chain.contains(op)) {
            visited += op
            val ins = opInBufferOrder(op)
            val tvalName = ju.inputSearch(ins, 1)
            val fvalName = ju.inputSearch(ins, 2)
            val preOps = outEdgeBuffer.getOrElse(tvalName, HashSet.empty) ++
                        outEdgeBuffer.getOrElse(fvalName, HashSet.empty)
            preOps.foreach(dfs)
            stack += op
          }
        }

        dfs(root)
        if (stack.size != chain.size) {
          println("root: ", root)
          println(s"chain: ", chain)
          println(s"stack: ", stack)

          chain.foreach {op=>{
            val ins = opInBufferOrder(op)
            val tvalName = ju.inputSearch(ins, 1)
            val fvalName = ju.inputSearch(ins, 2)
            val preOps = outEdgeBuffer.getOrElse(tvalName, HashSet.empty) ++
                        outEdgeBuffer.getOrElse(fvalName, HashSet.empty)
            println(s"  op: $op preOps: ${preOps.mkString(", ")}")
          }}

          throw new Exception(s"Topological sort failed: visited ${stack.size}, chain size ${chain.size}")
        }
        stack.reverse.toSeq // root first
      }

      if (chainSize <= limit) {
        ret(root) = ArrayBuffer.from(topoSort(root, chain))
      } else {
        val splitSizes = splitNumber(chainSize, limit)
        val sortedOps = topoSort(root, chain)
        var index = 0
        splitSizes.foreach { size =>
          val subChain = ArrayBuffer[String]()
          for (i <- 0 until size) {
            subChain += sortedOps(index)
            index += 1
          }
          val subRoot = subChain.headOption.getOrElse("")
          if (subRoot.nonEmpty) {
            ret(subRoot) = subChain
          }
        }
      }
      ret
    }

    def isContinuous(buf: ArrayBuffer[Int]): (Boolean, Boolean) = {
      if (buf.length <= 1) return (true, true)

      val diffs = buf.zip(buf.tail).map { case (a, b) => b - a }

      (diffs.forall(_ == 1) || diffs.forall(_ == -1), diffs.forall(_ == -1))
    }

    def bitCondFinalCheck(root: String, chain: ArrayBuffer[String]): Boolean = {
      val varInSet = HashSet[String]()
      var shiftIdx = ArrayBuffer[Int]()
      chain.foreach { op =>
        val ins = opInBufferOrder(op)
        val condName = ju.inputSearch(ins, 0)
        val preOps = outEdgeBuffer.getOrElse(condName, HashSet.empty)
        val preOp = preOps.headOption.getOrElse("")
        if (inEdgeBuffer.getOrElse(condName, HashSet.empty).size != 1) {
          println(s"  bitCondFinalCheck: cond $condName has multiple inEdges: ${inEdgeBuffer.getOrElse(condName, HashSet.empty).mkString(", ")}")
          return false
        }
        if (preOp.nonEmpty) {
          val preOpIns = opInBufferOrder(preOp)
          val varIn = ju.inputSearch(preOpIns, 0)
          varInSet += varIn
          val maskBit = ju.inputSearch(preOpIns, 2).toInt
          if (maskBit != 1)
            return false
          val shiftBit = ju.inputSearch(preOpIns, 1).toInt
          shiftIdx += shiftBit
        }
      }
      if (varInSet.size != 1) {
        println(s"  bitCondFinalCheck: multiple input vars found: ${varInSet.mkString(", ")}")
        return false 
      }
      // check if shiftIdx is already continuous
      // can either in ascending or descending order
      if (!isContinuous(shiftIdx)._1) {
        println(s"  bitCondFinalCheck: shiftIdx not continuous: ${shiftIdx.mkString(", ")}")
        return false
      }
      bitChain_order(root) = isContinuous(shiftIdx)._2 // true if descending
      true
    }

    def finalChainSplit(chains: HashMap[Int, HashMap[String, ArrayBuffer[String]]]): HashMap[Int, HashMap[String, ArrayBuffer[String]]] = {
      val bitChains = chains.getOrElse(2, HashMap[String, ArrayBuffer[String]]())
      val nonBitChains = chains.getOrElse(0, HashMap[String, ArrayBuffer[String]]())
      val bitRet = HashMap[String, ArrayBuffer[String]]()
      val nonBitRet = HashMap[String, ArrayBuffer[String]]()
      val ret = HashMap[Int, HashMap[String, ArrayBuffer[String]]]()

      bitChains.foreach { case (root, chain) =>
        val splitChains = splitLongChain(root, chain)
        splitChains.foreach { case (subRoot, subChainBuf) =>
          if (subChainBuf.size > 2) {
            if (bitCondFinalCheck(subRoot, subChainBuf)) {
              bitRet(subRoot) = subChainBuf
            } else {
              nonBitRet(subRoot) = subChainBuf
            }
          }
        }
      }

      nonBitChains.foreach { case (root, chain) =>
        val splitChains = splitLongChain(root, chain)
        splitChains.foreach { case (subRoot, subChainBuf) =>
          if (subChainBuf.size > 2) {
            nonBitRet(subRoot) = subChainBuf
          }
        }
      }
      
      ret(2) = bitRet
      ret(0) = nonBitRet
      ret
    }

    def randomIdxJT(chains: HashMap[String, ArrayBuffer[String]]): HashMap[ArrayBuffer[String], ArrayBuffer[String]] = {
      val muxChainInfo = HashMap[ArrayBuffer[String], ArrayBuffer[String]]()
      chains foreach {case(root, chain)=>{
        val bitWidth = chain.size
        val tableSize = math.pow(2, bitWidth).toInt
        val jump_array = ArrayBuffer.fill(tableSize)("")

        // recursive helper
        def fillJumpArray(level: Int, start: Int, end: Int, tval: String, fval: String): Unit = {
          if (level >= bitWidth) return
          val mid = (start + end) / 2
          for (i <- start until mid) jump_array(i) = fval
          for (i <- mid until end) jump_array(i) = tval
        }

        // Build recursively through the chain
        def fillChain(idx: Int, start: Int, end: Int): Unit = {
          if (idx >= chain.size) return
          val op = chain(idx)
          val ins = opInBufferOrder(op)
          val tvalName = ju.inputSearch(ins, 1)
          val fvalName = ju.inputSearch(ins, 2)
          val tvalpreOp = outEdgeBuffer.getOrElse(tvalName, HashSet.empty).headOption
          val fvalpreOp = outEdgeBuffer.getOrElse(fvalName, HashSet.empty).headOption

          // Determine which side continues the chain
          val nextOp = if (idx + 1 < chain.size) Some(chain(idx + 1)) else None
          (tvalpreOp, fvalpreOp, nextOp) match {
            case (Some(tOp), _, Some(nOp)) if tOp == nOp =>
              // tval is previous op → upper half = tval, lower = fval
              fillJumpArray(idx, start, end, tvalName, fvalName)
              // fillChain(idx + 1, start, (start + end) / 2)
              fillChain(idx + 1, (start + end) / 2, end)
            case (_, Some(fOp), Some(nOp)) if fOp == nOp =>
              // fval is next op → upper half = tval, lower = fval
              fillJumpArray(idx, start, end, tvalName, fvalName)
              // fillChain(idx + 1, (start + end) / 2, end)
              fillChain(idx + 1, start, (start + end) / 2)
            case _ =>
              // leaf mux
              fillJumpArray(idx, start, end, tvalName, fvalName)
          }
        }

        // Start recursion
        fillChain(0, 0, tableSize)

        // Optionally record results
        muxChainInfo += (chain -> jump_array)
      }}
      muxChainInfo
    }

    def addCondMux(outName: String, new_op: String, condVar: String, jump_idx: String): Unit = {
      val mask = (BigInt(1) << bitWidthMetadata(condVar))-1
      inEdgeBuffer.getOrElseUpdate(condVar, HashSet.empty) += new_op
      inEdgeBuffer.getOrElseUpdate(jump_idx, HashSet.empty) += new_op
      inEdgeBuffer.getOrElseUpdate(mask.toString, HashSet.empty) += new_op
      outEdgeBuffer(outName) = HashSet(new_op)
      oEBufferR(new_op) = outName
      opInBufferOrder(new_op) = HashMap(condVar->HashSet(0))
      opInBufferOrder(new_op).getOrElseUpdate(mask.toString, HashSet.empty) += 1
      opInBufferOrder(new_op).getOrElseUpdate(jump_idx, HashSet.empty) += 2
      bitWidthMetadata(new_op) = bitWidthMetadata(outName)
      bitWidthMetadata(mask.toString) = 64
    }

    def updateJumpTable(): Unit = {
      for (i <- muxJumpTable.indices) {
        if (outEdgeBuffer.contains(muxJumpTable(i))) {
          val inOp = outEdgeBuffer(muxJumpTable(i)).head
          muxJumpTable(i) = inOp
        } 
      }
    }

    def orChainExtract(): HashSet[HashSet[Int]] = {
      var orExtract = HashSet[Int]()
      for (count <- 0 until op_counter) {
        val operation = s"or_+_${count}"
        if (opInBufferOrder.contains(operation)) {
          orExtract += count
        }
      }

      val sortedKeys = orExtract.toArray.sorted

      val groupedContiguousKeys = HashSet[HashSet[Int]]()

      for (or_count <- sortedKeys) {
        val op = s"or_+_${or_count}"
        var inserted = false

        for ((key, _) <- opInBufferOrder(op)) {
          if (outEdgeBuffer.contains(key)) {
            val in_op = outEdgeBuffer(key).head
            if (in_op.startsWith("or_+_") && opInBufferOrder.contains(in_op)) {
              val cur_count = in_op.replace("or_+_", "").toInt
              for (group <- groupedContiguousKeys) {
                // println(group)
                if (!inserted) {
                  if (group.contains(cur_count)) {
                    // println("here?", group)
                    group += or_count
                    // println("after", group)
                    inserted = true
                  }
                }
              }
            }
          }
        }

        if (!inserted) {
          groupedContiguousKeys += HashSet(or_count)
        }
      }

      // Step 3: Filter groups of size >= 2 and convert to HashSet[HashSet[String]]
      var orClsuer = HashSet[HashSet[Int]]()

      for (group <- groupedContiguousKeys if group.size >= 2) {
        orClsuer += group
      }

      // println("Or Chain Extracted: ", orClsuer)
      var filter_orClsuer = HashSet[HashSet[Int]]()
      orClsuer foreach {cluster=>{
        val sortedCluster = cluster.toSeq.sorted
        var filter_cluster = HashSet[Int]()
        for ((elem, next) <- sortedCluster.zip(sortedCluster.tail)) {
          val opName = s"or_+_${elem}"
          val output = oEBufferR(opName)
          val next_op = s"or_+_${next}"
          val next_inputs = opInBufferOrder(next_op)
          filter_cluster += elem
          if ((!next_inputs.contains(output)) || inEdgeBuffer(output).size > 1) {
            // remove elem 
            filter_orClsuer += filter_cluster
            filter_cluster = HashSet[Int]()
          }
        }
        if (filter_cluster.nonEmpty) {
          filter_orClsuer += filter_cluster
        }
      }}

      val filter_orCluster = filter_orClsuer.filter(_.size >= 2)

      // println("Filtered Or Chain Extracted: ", filter_orCluster)
      filter_orCluster
    }


    def addOrChain(filter_cluster: HashSet[HashSet[Int]]): Unit = {
      filter_cluster foreach {cluster =>{
        val new_op = s"orchain_+_${op_counter}"
        opInBufferOrder += new_op->HashMap.empty[String, HashSet[Int]]
        op_counter += 1
        val sorted_cluster = cluster.toSeq.sorted
        // println("Adding Or Chain: ", sorted_cluster)
        // println(sorted_cluster.size.toString)
        // println(sorted_cluster.init)
        // println(sorted_cluster.last)
        opInBufferOrder(new_op).getOrElseUpdate((sorted_cluster.size+1).toString, HashSet.empty) += 0
        inEdgeBuffer.getOrElseUpdate((sorted_cluster.size+1).toString, HashSet.empty) += new_op
        var input_idx = 1
        var intermediate_output = HashSet[String]()
        sorted_cluster.init foreach {elem=>{
          val opName = s"or_+_${elem}"
          val output = oEBufferR(opName)
          val inputs = opInBufferOrder(opName)
          inputs foreach {case(in, idx)=>{
            inEdgeBuffer(in) -= opName
            if (!intermediate_output.contains(in)) {
              inEdgeBuffer(in) += new_op

              opInBufferOrder(new_op).getOrElseUpdate(in, HashSet.empty) += input_idx
              input_idx += 1
            }
            if (inEdgeBuffer(in).isEmpty) {
              inEdgeBuffer.remove(in)
            }
          }}
          oEBufferR.remove(opName)
          outEdgeBuffer.remove(output)
          intermediate_output += output
          opInBufferOrder.remove(opName)
        }}

        val lastElem = sorted_cluster.last
        val opName = s"or_+_${lastElem}"
        val output = oEBufferR(opName)
        val inputs = opInBufferOrder(opName)
        inputs foreach {case(in, idx)=>{
          inEdgeBuffer(in) -= opName
          if (!intermediate_output.contains(in)) {
            inEdgeBuffer(in) += new_op

            opInBufferOrder(new_op).getOrElseUpdate(in, HashSet.empty) += input_idx
            input_idx += 1
          }
          if (inEdgeBuffer(in).isEmpty) {
            inEdgeBuffer.remove(in)
          }
        }}
        outEdgeBuffer(output) = HashSet(new_op)
        oEBufferR(new_op) = output
        opInBufferOrder.remove(opName)

        bitWidthMetadata(new_op) = bitWidthMetadata(output)
      }}
    }

    def xorChainExtract(): HashSet[HashSet[Int]] = {
      var orExtract = HashSet[Int]()
      for (count <- 0 until op_counter) {
        val operation = s"xor_+_${count}"
        if (opInBufferOrder.contains(operation)) {
          orExtract += count
        }
      }

      val sortedKeys = orExtract.toArray.sorted
      val groupedContiguousKeys = HashSet[HashSet[Int]]()

      for (or_count <- sortedKeys) {
        val op = s"xor_+_${or_count}"
        var inserted = false

        for ((key, _) <- opInBufferOrder(op)) {
          if (outEdgeBuffer.contains(key)) {
            val in_op = outEdgeBuffer(key).head
            if (in_op.startsWith("xor_+_") && opInBufferOrder.contains(in_op)) {
              val cur_count = in_op.replace("xor_+_", "").toInt
              for (group <- groupedContiguousKeys) {
                // println(group)
                if (!inserted) {
                  if (group.contains(cur_count)) {
                    // println("here?", group)
                    group += or_count
                    // println("after", group)
                    inserted = true
                  }
                }
              }
            }
          }
        }

        if (!inserted) {
          groupedContiguousKeys += HashSet(or_count)
        }
      }

      // Step 3: Filter groups of size >= 2 and convert to HashSet[HashSet[String]]
      var orClsuer = HashSet[HashSet[Int]]()

      for (group <- groupedContiguousKeys if group.size >= 2) {
        orClsuer += group
      }

      // println("Or Chain Extracted: ", orClsuer)
      var filter_orClsuer = HashSet[HashSet[Int]]()
      orClsuer foreach {cluster=>{
        val sortedCluster = cluster.toSeq.sorted
        var filter_cluster = HashSet[Int]()
        for ((elem, next) <- sortedCluster.zip(sortedCluster.tail)) {
          val opName = s"xor_+_${elem}"
          val output = oEBufferR(opName)
          val next_op = s"xor_+_${next}"
          val next_inputs = opInBufferOrder(next_op)
          filter_cluster += elem
          if ((!next_inputs.contains(output)) || inEdgeBuffer(output).size > 1) {
            // remove elem 
            filter_orClsuer += filter_cluster
            filter_cluster = HashSet[Int]()
          }
        }
        if (filter_cluster.nonEmpty) {
          filter_orClsuer += filter_cluster
        }
      }}

      val filter_orCluster = filter_orClsuer.filter(_.size >= 2)

      // println("Filtered Or Chain Extracted: ", filter_orCluster)
      filter_orCluster
    }


    def addXorChain(filter_cluster: HashSet[HashSet[Int]]): Unit = {
      filter_cluster foreach {cluster =>{
        val new_op = s"xorchain_+_${op_counter}"
        opInBufferOrder += new_op->HashMap.empty[String, HashSet[Int]]
        op_counter += 1
        val sorted_cluster = cluster.toSeq.sorted
        // println("Adding Or Chain: ", sorted_cluster)
        // println(sorted_cluster.size.toString)
        // println(sorted_cluster.init)
        // println(sorted_cluster.last)
        opInBufferOrder(new_op).getOrElseUpdate((sorted_cluster.size+1).toString, HashSet.empty) += 0
        inEdgeBuffer.getOrElseUpdate((sorted_cluster.size+1).toString, HashSet.empty) += new_op
        var input_idx = 1
        var intermediate_output = HashSet[String]()
        sorted_cluster.init foreach {elem=>{
          val opName = s"xor_+_${elem}"
          val output = oEBufferR(opName)
          val inputs = opInBufferOrder(opName)
          inputs foreach {case(in, idx)=>{
            inEdgeBuffer(in) -= opName
            if (!intermediate_output.contains(in)) {
              inEdgeBuffer(in) += new_op

              opInBufferOrder(new_op).getOrElseUpdate(in, HashSet.empty) += input_idx
              input_idx += 1
            }
            if (inEdgeBuffer(in).isEmpty) {
              inEdgeBuffer.remove(in)
            }
          }}
          oEBufferR.remove(opName)
          outEdgeBuffer.remove(output)
          intermediate_output += output
          opInBufferOrder.remove(opName)
        }}

        val lastElem = sorted_cluster.last
        val opName = s"xor_+_${lastElem}"
        val output = oEBufferR(opName)
        val inputs = opInBufferOrder(opName)
        inputs foreach {case(in, idx)=>{
          inEdgeBuffer(in) -= opName
          if (!intermediate_output.contains(in)) {
            inEdgeBuffer(in) += new_op

            opInBufferOrder(new_op).getOrElseUpdate(in, HashSet.empty) += input_idx
            input_idx += 1
          }
          if (inEdgeBuffer(in).isEmpty) {
            inEdgeBuffer.remove(in)
          }
        }}
        outEdgeBuffer(output) = HashSet(new_op)
        oEBufferR(new_op) = output
        opInBufferOrder.remove(opName)

        bitWidthMetadata(new_op) = bitWidthMetadata(output)
      }}
    }

    def repeatedOps(): Unit = {
      var opTosingle = HashMap[String, String]()
      var singleToRvmOp = HashMap[String, HashSet[String]]()

      val rvmOp = HashMap.empty[HashMap[String, HashSet[Int]], HashMap[String, HashSet[String]]]
      
      opInBufferOrder .foreach { case (opName, keyMap) =>
        val output = if (oEBufferR.contains(opName)) oEBufferR(opName) else ""
        if (!SimDTM_output.contains(output)) {
          val op_type = opName.split("_\\+_")(0) 
          val output = if (oEBufferR.contains(opName)) oEBufferR(opName) else ""
          if (op_type != "memw" && op_type != "memr" && op_type != "dshl" && op_type != "dshr" && op_type != "dshrS" && op_type != "longbits" && !op_type.contains("chain") && !output.contains("@next")) {
            if (rvmOp.contains(keyMap)) {
              if (rvmOp(keyMap).contains(op_type))
                rvmOp(keyMap)(op_type) += opName
              else
                rvmOp(keyMap) += op_type->HashSet(opName)
            }
            else {
              val oplist = HashMap(op_type->HashSet(opName))
              rvmOp += (keyMap).clone()->oplist
            }
          }
        }
      }

      rvmOp.foreach { case (_, opTypes) =>
        opTypes.foreach { case (opType, opsSet) =>
          if (opsSet.size>1) {
            val new_op = s"${opType}_+_${op_counter }"
            singleToRvmOp += new_op->opsSet.clone()
            op_counter  = op_counter  + 1

            opsSet foreach {rop=>{
              opTosingle += rop->new_op
            }}
          }
        }
      }

      opTosingle foreach {case(op, newop)=>{
        bitWidthMetadata  += newop->bitWidthMetadata(op)
      }}

      inEdgeBuffer .keys.foreach { ins =>
        val oldOps = inEdgeBuffer (ins)
        val updatedOps = oldOps.map { op =>
          if (opTosingle.contains(op)) opTosingle(op) else op
        }
        inEdgeBuffer(ins) = updatedOps
      }

      var removeIOuts = HashSet[String]()
      var addOuts = HashMap[String, HashSet[String]]()
      outEdgeBuffer .keys.foreach { ins =>
        val op = outEdgeBuffer (ins).head
        if (opTosingle.contains(op)) {
          removeIOuts += ins
          addOuts += ins->HashSet(opTosingle(op))
        }
      }
      removeIOuts foreach {out=>{
        outEdgeBuffer .remove(out)
      }}
      addOuts foreach{case(in, op)=>{
        outEdgeBuffer  += in->op.clone()
      }}
      oEBufferR  foreach {case(op, out)=>{
        if (opTosingle.contains(op)) {
          oEBufferR  += opTosingle(op)->oEBufferR (op)
          oEBufferR .remove(op)
        }
      }}

      val toRemove = HashSet[String]()
      val toAdd = HashMap[String, HashMap[String, HashSet[Int]]]()

      opInBufferOrder .foreach { case (op, keyMap) =>
        if (opTosingle.contains(op)) {
          toRemove += op

          val deepClone = HashMap[String, HashSet[Int]]()
          keyMap.foreach { case (k, vSet) =>
            deepClone(k) = HashSet(vSet.toSeq: _*)
          }
          toAdd += (opTosingle(op) -> deepClone)
          bitWidthMetadata  += opTosingle(op) -> bitWidthMetadata (op)
        }
      }
      toRemove.foreach(opInBufferOrder  -= _)
      toAdd.foreach { case (k, v) => opInBufferOrder  += (k -> v) }
      uniqueOps = opInBufferOrder.size
    }


    def tconstp_all_func(): Unit = {
      constExtract()
      constProp_rec()

      deadRegElimFixpoint()
      nonUsedRegElim()

      val mux_roots = findMuxChainRoot()

      var chain_count = HashSet[String]()
      val chains = chainTraceBack(mux_roots)
      var chain_length = 0
      println("mux chains: ", chains.size)
      chains foreach {case(root, chain)=>{
        // println("size for this chain: ", chain.size)
        chain_count ++= chain
        chain_length += chain.size
        // println("mux chain root: ", root)
        // println("mux chain: ", chain)
        // println("\n")
      }}
      println("total mux in chains: ", chain_count.size, "chain length count: ", chain_length)

      val classed_chains = chainClassification(chains)
      println("random chains: ", classed_chains(0).size)
      println("case chains: ", classed_chains(1).size)

      // Adjust chain by removing chains that have muxes also appearing in other chains
      val adjusted_chain_class = classAdjust(classed_chains)
      // break branch mux
      val branchlessChain = HashMap[String, HashSet[String]]()
      adjusted_chain_class(0) foreach { case(root, chain) => {
        branchlessChain ++= breakBranchMux(root, chain)
      }}

      branchlessChain.filterInPlace { case (_, chain) =>
        chain.size > 2
      }

      println("broken adjusted random chains: ", branchlessChain.size)
      println("adjusted case chains: ", adjusted_chain_class(1).size)

      var adjusted_avg_random_chain_size = 0.0
      var adjusted_random_chain_len = HashMap[Int, Int]()
      branchlessChain foreach { case(root, chain) => {
        adjusted_avg_random_chain_size += chain.size
        adjusted_random_chain_len(chain.size) = adjusted_random_chain_len.getOrElse(chain.size, 0) + 1
        // println("random chain root: ", root)
        // println("random chain: ", chain)
        // println("\n")
      }}
      if (branchlessChain.nonEmpty)
        adjusted_avg_random_chain_size = adjusted_avg_random_chain_size/branchlessChain.size
      println("adjusted avg random chain size: ", adjusted_avg_random_chain_size)
      println("adjusted random chain length distribution: ", adjusted_random_chain_len.toSeq.sorted)

      println("Checking adjusted random chains...")
      branchlessChain foreach {case(root, chain)=>{
        if (!randomChainCheck(root, chain)) {
          println("random chain root failed: ", root)
          println("random chain failed: ", chain)
          chain foreach {mux=>{
            val condName = ju.inputSearch(opInBufferOrder(mux), 0)
            val tvalName = ju.inputSearch(opInBufferOrder(mux), 1)
            val fvalName = ju.inputSearch(opInBufferOrder(mux), 2)
            val outName = oEBufferR(mux)

            println("mux: ", mux)
            println("cond: ", condName)
            println("tval: ", tvalName)
            println("fval: ", fvalName)
            println("outName: ", outName)
            println(inEdgeBuffer.getOrElse(outName, HashSet.empty))
            println("\n")
          }}
        }
      }}

      val random_chain_stat = stringChainStat(branchlessChain)
      println("adjusted random chain mux string length distribution: ", random_chain_stat)

      val bitCondSeparation = bitCondExtract(branchlessChain)

      val shortChains = finalChainSplit(bitCondSeparation)

      println("splitted short random chains: ", shortChains(0).size)
      println("splitted short bits chains: ", shortChains(2).size)

      var short_avg_random_chain_size = 0.0
      var short_random_chain_len = HashMap[Int, Int]()
      shortChains(0) foreach { case(root, chain) => {
        short_avg_random_chain_size += chain.size
        short_random_chain_len(chain.size) = short_random_chain_len.getOrElse(chain.size, 0) + 1
        // println("random chain root: ", root)
        // println("random chain: ", chain)
        // println("\n")
      }}
      if (shortChains(0).nonEmpty)
        short_avg_random_chain_size = short_avg_random_chain_size/shortChains(0).size
      println("splitted short random chain size: ", short_avg_random_chain_size)
      println("splitted short random chain length distribution: ", short_random_chain_len.toSeq.sorted)

      println("Checking splitted short random chains...")
      shortChains(0) foreach {case(root, chain)=>{
        if (!randomChainCheck(root, chain.to(HashSet))) {
          println("splitted short random chain root failed: ", root)
          println("splitted short random chain failed: ", chain)
          chain foreach {mux=>{
            val condName = ju.inputSearch(opInBufferOrder(mux), 0)
            val tvalName = ju.inputSearch(opInBufferOrder(mux), 1)
            val fvalName = ju.inputSearch(opInBufferOrder(mux), 2)
            val outName = oEBufferR(mux)

            println("mux: ", mux)
            println("cond: ", condName)
            println("tval: ", tvalName)
            println("fval: ", fvalName)
            println("outName: ", outName)
            println(inEdgeBuffer.getOrElse(outName, HashSet.empty))
            println("\n")
          }}
        }
      }}


      var short_avg_bits_chain_size = 0.0
      var short_bits_chain_len = HashMap[Int, Int]()
      shortChains(2) foreach { case(root, chain) => {
        short_avg_bits_chain_size += chain.size
        short_bits_chain_len(chain.size) = short_bits_chain_len.getOrElse(chain.size, 0) + 1
        // println("random chain root: ", root)
        // println("random chain: ", chain)
        // println("\n")
      }}
      if (shortChains(2).nonEmpty)
        short_avg_bits_chain_size = short_avg_bits_chain_size/shortChains(2).size
      println("splitted short bits chain size: ", short_avg_bits_chain_size)
      println("splitted short bits chain length distribution: ", short_bits_chain_len.toSeq.sorted)

      println("Checking splitted short bits chains...")
      shortChains(2) foreach {case(root, chain)=>{
        if (!randomChainCheck(root, chain.to(HashSet))) {
          println("splitted short bits chain root failed: ", root)
          println("splitted short bits chain failed: ", chain)
          chain foreach {mux=>{
            val condName = ju.inputSearch(opInBufferOrder(mux), 0)
            val tvalName = ju.inputSearch(opInBufferOrder(mux), 1)
            val fvalName = ju.inputSearch(opInBufferOrder(mux), 2)
            val outName = oEBufferR(mux)

            println("mux: ", mux)
            println("cond: ", condName)
            println("tval: ", tvalName)
            println("fval: ", fvalName)
            println("outName: ", outName)
            println(inEdgeBuffer.getOrElse(outName, HashSet.empty))
            println("\n")
          }}
        }
      }}

      // random chain
      val chain_JT_pair_bits = randomIdxJT(shortChains(2))


      var adjusted_avg_case_chain_size = 0.0
      var adjusted_case_chain_len = HashMap[Int, Int]()
      adjusted_chain_class(1) foreach {case(root, chain)=>{
        adjusted_avg_case_chain_size += chain.size
        adjusted_case_chain_len(chain.size) = adjusted_case_chain_len.getOrElse(chain.size, 0) + 1
        // println("case chain root: ", root)
        // println("case chain: ", chain)
        // println("\n")
      }}
      if (adjusted_chain_class(1).nonEmpty)
        adjusted_avg_case_chain_size = adjusted_avg_case_chain_size/adjusted_chain_class(1).size
      println("adjusted avg case chain size: ", adjusted_avg_case_chain_size)
      println("adjusted case chain length distribution: ", adjusted_case_chain_len.toSeq.sorted)

      println("Checking adjusted case chains...")
      adjusted_chain_class(1) foreach {case(root, chain)=>{
        if (!caseChainChecker(root, chain)) {
          println("case chain root failed: ", root)
          println("case chain failed: ", chain)
        }
      }}

      val new_muxRecordPair = muxRecordReform(adjusted_chain_class(1))
      val new_muxChainInfo = newMuxChainInfoExtract(adjusted_chain_class(1), new_muxRecordPair._1, new_muxRecordPair._2)
      new_updateMuxChain(new_muxChainInfo, new_muxRecordPair._1, new_muxRecordPair._2)

      val filter_orClsuer = orChainExtract()
      addOrChain(filter_orClsuer)
      val filter_xorClsuer = xorChainExtract()
      addXorChain(filter_xorClsuer)
      repeatedOps()
      
      updateJumpTable()

      constPropagation.clear()
      muxRecord.clear()
      muxCondRecord.clear()
      muxChainForm.clear()
      muxChainCollect.clear()
      muxChainInfo.clear()
    }
}


object TeAALDFGraphConstProp {
    var op_c = 0

    def update_op(op_c_te: Int): Unit = {
        op_c = op_c_te
    }
}