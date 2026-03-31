package essent

import java.io.{File, FileWriter, Writer, PrintWriter}
import java.nio.file.{Paths, Files}
import collection.mutable.{HashMap, LinkedHashMap}
import org.yaml.snakeyaml.Yaml
import scala.jdk.CollectionConverters._
import scala.collection.mutable.{Map => MMap}
import java.nio.charset.StandardCharsets
import upickle.default._

import essent.TeAALDFGraphExtractor._
import essent.ir._
import firrtl._
import firrtl.ir._

import collection.mutable.{ArrayBuffer, HashMap, HashSet}
import scala.collection.immutable.{SortedMap, TreeMap}


class TeAALTranspiler(opIndex: HashMap[String, Int]) {

  import TeAALTranspiler._

  var layerNum = 0
  var opNum = 0

  def extractDim(nodeIDOrderMap: teAALIDHashMap): Unit = {
    layerNum = nodeIDOrderMap.size 
    nodeIDOrderMap foreach { layer => {
      opNum = opNum max layer._2.size
    }}
  }

  def dtmSignal(dtmSignalCollect: HashMap[String, Int], outputDir: String, tName: String): Unit = {
    val dir = new File(outputDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val jsonFile = new File(outputDir, s"${tName}.json")
    val witer = new FileWriter(jsonFile)

    val dtm = new StringBuilder()

    witer.write("{\n")
    val entries = dtmSignalCollect.toSeq.map { case (k, v) =>
      s"""  "${k}": $v"""
    }
    witer.write(entries.mkString(",\n"))
    witer.write("\n}\n")

    witer.close()
  }

  def InputRegMap(input_idx_map: HashMap[String, ArrayBuffer[Int]], outputDir: String, tName: String): Unit = {
    val dir = new File(outputDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val immutableMap: Map[String, Seq[Int]] = input_idx_map.view.mapValues(_.toSeq).toMap
    val jsonStr: String = write(immutableMap, indent = 2)
    val filePath = Paths.get(outputDir, s"${tName}.json")
    Files.createDirectories(filePath.getParent)
    Files.write(filePath, jsonStr.getBytes(StandardCharsets.UTF_8))
  }

  def debugSignal(debugSignalCollect: HashMap[Int, HashMap[Int, String]], outputDir: String, tName: String): Unit = {
    val dir = new File(outputDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val jsonFile = new File(outputDir, s"${tName}.json")
    val witer = new FileWriter(jsonFile)

    val dtm = new StringBuilder()

    witer.write("{\n")
    val layerEntries = debugSignalCollect.toSeq
      .map { case (layer, innerMap) =>
      val filtered = innerMap.toSeq
        .filterNot { case (_, varname) =>
          varname.contains("nestedNode") ||
          varname.contains("_+_") ||
          varname.contains("gated_clock_debug_clock_gate") ||
          varname.contains("SimDTM") ||
          varname.contains("io_success")
        }
        .map { case (index, varname) =>
          s"""    "$index": "$varname""""
        }

      if (filtered.nonEmpty) {
        val body = filtered.mkString(",\n")
        s"""  "$layer": {\n$body\n  }"""
      } else {
        s"""  "$layer": {}"""
      }
    }
    witer.write(layerEntries.mkString(",\n"))
    witer.write("\n}\n")

    witer.close()
  }

  def jumpTableGen(muxJumpTable: ArrayBuffer[String], layerNodeIDN: convHashMap, outputDir: String, tName: String): Unit = {
    val dir = new File(outputDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val tensorYML = new File(outputDir, s"${tName}.txt")
    val tensor = new FileWriter(tensorYML)
    var has = ArrayBuffer[String]()

    val muxJT = new StringBuilder()

    muxJumpTable foreach {jump_idx=>{
      val found = layerNodeIDN.exists { case (_, innerMap) =>
        innerMap.contains(jump_idx)
      }
      val result = layerNodeIDN.collectFirst {
        case (_, inner) if inner.contains(jump_idx) => jump_idx -> inner(jump_idx)
      }

      if (!found) {println("not found jump_idx: ", jump_idx)}
      // if (found) {
      muxJT.append(result.get._2.toString)
      muxJT.append(",")
      has.append(result.get._1)
      // }
    }}

    if (muxJT.endsWith(",")) {
      muxJT.setLength(muxJT.length - 1) // Remove last comma
    }
    val commaCount = muxJT.toString.count(_ == ',')

    muxJumpTable foreach {i=>{
      if (!has.contains(i)) {
        println(i)
      }
    }}
    
    if ((commaCount+1) != muxJumpTable.size) {
      println("length do not match!")
    }

    tensor.write(muxJT.toString)

    tensor.close()
  }

  def unrollInnerDimIn(nodeIDOrderMap: teAALIDHashMap, layerNum: Int, r2: Int, r3: Int): String = {
    val innerDimS = new StringBuilder()
    val innerR = new StringBuilder()

    innerDimS.append("            - fiber:\n")
    innerDimS.append("                coords:   [")
    
    val numofnodeinlayer = nodeIDOrderMap(layerNum).toSeq.sortBy(_._1)

    numofnodeinlayer.foreach { node =>
      val i = node._1
      innerDimS.append(i).append(", ")

      val rrank = nodeIDOrderMap(layerNum)(i).toSeq.sortBy(_._1)
      // innerR.clear()
      innerR.append("                - fiber:\n")
      innerR.append("                    coords:   [")

      val inn = new StringBuilder()

      rrank.foreach { coord =>
        innerR.append(coord._1).append(", ")
        inn.append(coord._2).append(", ")
      }

      if (innerR.endsWith(", ")) {
        innerR.setLength(innerR.length - 2) // Remove last comma
      }

      innerR.append("]\n")
      innerR.append("                    payloads:   [")
      
      if (inn.endsWith(", ")) {
        inn.setLength(inn.length - 2) // Remove last comma
      }

      inn.append("]\n")
      innerR.append(inn)

      // innerDimS.append(innerR) // If needed, uncomment this
    }

    if (innerDimS.endsWith(", ")) {
      innerDimS.setLength(innerDimS.length - 2) // Remove last comma
    }

    innerDimS.append("]\n")
    innerDimS.append("                payloads:\n")
    innerDimS.append(innerR)

    innerDimS.toString()
  }


  def unrollInnerDimOp(isnMap: ISNMap, layerNum: Int, r2: Int): String = {

    val innerDimS = new StringBuilder()
    innerDimS.append("            - fiber:\n")
    innerDimS.append("                coords:   [")

    isnMap(layerNum).toSeq.sortBy(_._1) foreach(node=>{
      innerDimS.append(node._1).append(", ")
    })
    if (isnMap(layerNum).isEmpty)
      println(layerNum, isnMap(layerNum).toSeq.sortBy(_._1))
    
    if (innerDimS.endsWith(", ")) {
      innerDimS.setLength(innerDimS.length - 2) // Remove last comma
    }

    innerDimS.append("]\n")
    innerDimS.append("                payloads:   [")

    isnMap(layerNum).toSeq.sortBy(_._1) foreach(node=>{
      innerDimS.append(node._2).append(", ")
    })

    if (innerDimS.endsWith(", ")) {
      innerDimS.setLength(innerDimS.length - 2) // Remove last comma
    }

    innerDimS.append("]\n")

    innerDimS.toString()
  }

  def tensorTocircInYML(circuitInput: ArrayBuffer[BigInt], outputDir: String, tName: String): Unit = {
    val dir = new File(outputDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val tensorYML = new File(outputDir, s"${tName}.txt")
    val tensor = new FileWriter(tensorYML)

    var yml = ""
    
    for (i <- 0 to circuitInput.size-1) {
        yml += (circuitInput(i)).toString
        yml += ","
    }

    yml = yml.dropRight(1)

    tensor.write(yml)

    tensor.close()
  }

  def tensorToInYML(nodeIDOrderMap: teAALIDHashMap, r1: Int, r2: Int, r3: Int, rName1: String, rName2: String, rName3: String, outputDir: String, tName: String): Unit = {
    val dir = new File(outputDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val tensorYML = new File(outputDir, s"${tName}.yaml")
    val tensor = new FileWriter(tensorYML)
    if (r1 != layerNum) throw new Exception("Need to check layer number!")

    var dim2 = r2 max opNum

    var yml = "tensor:\n"
    yml += s"    name: ${tName}\n"
    yml += s"    rank_ids: [ $rName1, $rName2, $rName3 ]\n"
    yml += s"    shape: [ $r1, $dim2, $r3 ]\n"
    yml += "    root:\n"
    yml += "        - fiber:\n"
    yml += "            coords: ["
    
    for (i <- 0 to nodeIDOrderMap.size-1) {
        yml += i.toString
        yml += ", "
    }
    val idx = yml.lastIndexOf(", ")
    yml = yml.substring(0, idx)
    yml += "]\n"
    yml += "            payloads:\n"

    for (i <- 0 to nodeIDOrderMap.size-1) {
        yml += unrollInnerDimIn(nodeIDOrderMap, i, dim2, r3)
    }

    tensor.write(yml)

    tensor.close()
  }

  def tensorToOpYML(isnMap: ISNMap, r1: Int, r2: Int, rName1: String, rName2: String, outputDir: String, tName: String): Unit = {
    val dir = new File(outputDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val tensorYML = new File(outputDir, s"${tName}.yaml")
    val tensor = new FileWriter(tensorYML)
    // extractDim(nodeIDOrderMap)
    if (r1 != layerNum) throw new Exception("Need to check layer number!")

    var dim2 = r2 max opNum

    var yml = "tensor:\n"
    yml += s"    name: ${tName}\n"
    yml += s"    rank_ids: [ $rName1, $rName2 ]\n"
    yml += s"    shape: [ $r1, $dim2 ]\n"
    yml += "    root:\n"
    yml += "        - fiber:\n"
    yml += "            coords: ["
    
    for (i <- 0 to isnMap.size-1) {
        yml += i.toString
        yml += ", "
    }
    val idx = yml.lastIndexOf(", ")
    yml = yml.substring(0, idx)
    yml += "]\n"
    yml += "            payloads:\n"

    for (i <- 0 to isnMap.size-1) {
        yml += unrollInnerDimOp(isnMap, i, dim2)
        // isnMap.remove(i)
    }

    tensor.write(yml)

    tensor.close()
  }

  def shiftBlockCount(layerNum: Int, ops: HashSet[Int], nodeIDOrderMap: teAALIDHashMap): Int = {
    var shift_count = 0
    ops foreach {op=>{
      if (nodeIDOrderMap(layerNum).contains(op)) {
        shift_count += 1
      }
    }}
    shift_count
  }

  def kernels_gen(isnMap: ISNMap, nodeIDOrderMap: teAALIDHashMap, circuitInput: ArrayBuffer[BigInt], outputDir: String, tName: String): Unit = {
    IU_gen(isnMap, nodeIDOrderMap, outputDir, tName)
    SU_gen(isnMap, nodeIDOrderMap, circuitInput, outputDir, tName)
    TI_gen(isnMap, nodeIDOrderMap, circuitInput, outputDir, tName)
  }

  def sortOutMap(OutMap: HashMap[Int, HashMap[Int, Int]], OptypeMap: HashMap[Int, Int]): (HashMap[Int, Int], HashSet[Int]) = {
    // (layer, outputIdx, inputOrder, inputIdx)
    val result = HashMap[Int, Int]() // return outputIdx with order (skippable)
    val remaining = HashSet[Int]() // non-skippable
    case class Node(
      output: Int,
      inputs: HashMap[Int, Int] // (inputOrder, inputIdx)
    )
    val nodes = ArrayBuffer[Node]()
    val remainingInputs = HashMap[Int, Int]().withDefaultValue(0)
    var skipCount = 0 // number of operations could skip output buffer (directly write to data buffer)
    // Special treatment::
    // Dynamic shift: output is processed inside function -> cannot skip -> not added into nodes but remainingInputs
    // Mem Write: no output -> can skip
    // stop: no output -> can skip
    // mux chain: contain indirect from mux table -> cannot skip -> added into nodes
    for ((outIdx, inMap) <- OutMap) {
      if (OptypeMap(outIdx) == opIndex("dshl") || OptypeMap(outIdx) == opIndex("dshr") || OptypeMap(outIdx) == opIndex("dshrS") || OptypeMap(outIdx) == opIndex("chain")) {
        // dynamic shift op
        remaining += outIdx
      } else if (OptypeMap(outIdx) == opIndex("memw")) {
        // mem write op or stop op
        result += (skipCount -> outIdx)
        skipCount += 1
      } else {
        nodes += Node(outIdx, inMap)
        inMap.values.foreach { inIdx =>
          remainingInputs(inIdx) += 1
        }
      }
    }
    // Scheduling loop
    while (nodes.nonEmpty) {
      val readyIdx = nodes.indexWhere { n =>
        val outputDep = remainingInputs(n.output)
        if (outputDep == 0) {
          true
        } else if (outputDep == 1 && n.inputs.values.exists(_ == n.output)) {
          // The node’s output is also an input exactly once → allow scheduling
          true
        } else {
          false
        }
      }

      if (readyIdx == -1) {
        for (node <- nodes) {
          remaining += node.output
        }
        return (result, remaining) // No ready nodes found, exit the loop
      }

      val node = nodes.remove(readyIdx)

      result += (skipCount -> node.output)
      skipCount += 1

      // Emit
      node.inputs.values.foreach { inIdx =>
        remainingInputs(inIdx) -= 1
        if (remainingInputs(inIdx) == 0)
          remainingInputs -= inIdx
      }
    }
    (result, remaining)
  }

  def SU_Build(sb: StringBuilder, skip: Boolean, varflag: Boolean, opType: Int, outIdx: Int, circuitInput: ArrayBuffer[BigInt], inputMap: HashMap[Int, Int], layerOutCount: Int): Int = {
    var outputbufIdx = layerOutCount
    val databuf_str = if (varflag) "databuf_ptr_{}" else "databuf_ptr[{}]"
    val outbuf_str = if (varflag) "outbuf_ptr_{}" else "outbuf_ptr[{}]"
    opType match {
      case 0 => {
        // add
        val bwinfo = circuitInput(inputMap(0)).toInt
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = add_operation(${bwinfo}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)});\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = add_operation(${bwinfo}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)});\n")
        }
      }
      case 1 => {
        // sub
        val bwinfo = circuitInput(inputMap(0)).toInt
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = sub_operation(${bwinfo}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)});\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = sub_operation(${bwinfo}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)});\n")
        }
      }
      case 2 => {
        // mul, mulS, div, divS, rem
        val op_type = circuitInput(inputMap(0)).toInt
        if (skip) {
          if (op_type == 0) {
            sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(1).toString)} * ${databuf_str.replace("{}", inputMap(2).toString)};\n")
          } else if (op_type == 1) {
            sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}) * static_cast<int64_t>(${databuf_str.replace("{}", inputMap(2).toString)}));\n")
          } else if (op_type == 2) {
            sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(1).toString)} / ${databuf_str.replace("{}", inputMap(2).toString)};\n")
          } else if (op_type == 3) {
            sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}) / static_cast<int64_t>(${databuf_str.replace("{}", inputMap(2).toString)}));\n")
          } else if (op_type == 4) {
            sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(1).toString)} % ${databuf_str.replace("{}", inputMap(2).toString)};\n")
          }
        } else {
          if (op_type == 0) {
            sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(1).toString)} * ${databuf_str.replace("{}", inputMap(2).toString)};\n")
          } else if (op_type == 1) {
            sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}) * static_cast<int64_t>(${databuf_str.replace("{}", inputMap(2).toString)}));\n")
          } else if (op_type == 2) {
            sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(1).toString)} / ${databuf_str.replace("{}", inputMap(2).toString)};\n")
          } else if (op_type == 3) {
            sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}) / static_cast<int64_t>(${databuf_str.replace("{}", inputMap(2).toString)}));\n")
          } else if (op_type == 4) {
            sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(1).toString)} % ${databuf_str.replace("{}", inputMap(2).toString)};\n")
          }
        }
      }
      case 3 => {
        // asSInt
        val bwinfo = circuitInput(inputMap(1)).toInt
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = asSInt_operation(${databuf_str.replace("{}", inputMap(0).toString)}, ${bwinfo});\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = asSInt_operation(${databuf_str.replace("{}", inputMap(0).toString)}, ${bwinfo});\n")
        }
      }
      case 4 => 
        // lt
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} < ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} < ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 5 => 
        // ltS
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(0).toString)}) < static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}));\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(0).toString)}) < static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}));\n")
        }
      case 6 => 
        // leq
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} <= ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} <= ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 7 => 
        // leqS
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(0).toString)}) <= static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}));\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(0).toString)}) <= static_cast<int64_t>(${databuf_str.replace("{}", inputMap(1).toString)}));\n")
        }
      case 8 => 
        // eq
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} == ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} == ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 9 => 
        // neq
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} != ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} != ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 10 => 
        // shl
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} << ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} << ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 11 => 
        // shr
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} >> ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} >> ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 12 => 
        // shrS
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(0).toString)}) >> ${databuf_str.replace("{}", inputMap(1).toString)});\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = static_cast<uint64_t>(static_cast<int64_t>(${databuf_str.replace("{}", inputMap(0).toString)}) >> ${databuf_str.replace("{}", inputMap(1).toString)});\n")
        }
      case 13 => 
        // and
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} & ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} & ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 14 => 
        // or
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} | ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} | ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 15 => 
        // xor
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} ^ ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} ^ ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      case 16 => 
        // xorr
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = xorr_operation(${databuf_str.replace("{}", inputMap(0).toString)});\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = xorr_operation(${databuf_str.replace("{}", inputMap(0).toString)});\n")
        }
      case 17 => {
        // cat
        val shift = circuitInput(inputMap(2)).toInt
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = (${databuf_str.replace("{}", inputMap(0).toString)} << ${shift}) | ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = (${databuf_str.replace("{}", inputMap(0).toString)} << ${shift}) | ${databuf_str.replace("{}", inputMap(1).toString)};\n")
        }
      }
      case 18 => {
        // bits
        val shift = circuitInput(inputMap(1)).toInt
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = (${databuf_str.replace("{}", inputMap(0).toString)} >> ${shift}) & ${databuf_str.replace("{}", inputMap(2).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = (${databuf_str.replace("{}", inputMap(0).toString)} >> ${shift}) & ${databuf_str.replace("{}", inputMap(2).toString)};\n")
        }
      }
      case 19 => 
        // mux
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} ? ${databuf_str.replace("{}", inputMap(1).toString)} : ${databuf_str.replace("{}", inputMap(2).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)} ? ${databuf_str.replace("{}", inputMap(1).toString)} : ${databuf_str.replace("{}", inputMap(2).toString)};\n")
        }
      case 20 => 
        // assign
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${databuf_str.replace("{}", inputMap(0).toString)};\n")
        }
      case 21 => {
        // memr
        val mem_flag = circuitInput(inputMap(0)).toInt
        val offset = circuitInput(inputMap(1)).toInt
        val mask = circuitInput(inputMap(2)).toInt
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = memr_operation(${mem_flag}, ${offset}, ${databuf_str.replace("{}", inputMap(3).toString)}, ${mask}, memory_64_ptr, memory_8_ptr);\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = memr_operation(${mem_flag}, ${offset}, ${databuf_str.replace("{}", inputMap(3).toString)}, ${mask}, memory_64_ptr, memory_8_ptr);\n")
        }
      }
      case 22 => {
        // orchain
        var orchainString = ""
        for (i <- 1 to inputMap.size-1) {
          if (i == 1) {
            orchainString += s"${databuf_str.replace("{}", inputMap(i).toString)}"
          } else {
            orchainString += s" | ${databuf_str.replace("{}", inputMap(i).toString)}"
          }
        }
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${orchainString};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${orchainString};\n")
        }
      }
      case 23 => {
        // xorchain
        var xorchainString = ""
        for (i <- 1 to inputMap.size-1) {
          if (i == 1) {
            xorchainString += s"${databuf_str.replace("{}", inputMap(i).toString)}"
          } else {
            xorchainString += s" ^ ${databuf_str.replace("{}", inputMap(i).toString)}"
          }
        }
        if (skip) {
          sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = ${xorchainString};\n")
        } else {
          sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = ${xorchainString};\n")
        }
      }
      case 24 => {
        // muxchain
        assert (skip == false, "Mux Chain operation should not skip output buffer writing.")
        val mask = circuitInput(inputMap(1)).toInt
        val offset = circuitInput(inputMap(2)).toInt
        if (varflag) {
          if (skip) {
            sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = *data_mux_map[muxJT[(${databuf_str.replace("{}", inputMap(0).toString)} & ${mask}) + ${offset}]];\n")
          } else {
            sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = *data_mux_map[muxJT[(${databuf_str.replace("{}", inputMap(0).toString)} & ${mask}) + ${offset}]];\n")
          }
        } else {
          if (skip) {
            sb.append(s"${databuf_str.replace("{}", outIdx.toString)} = databuf_ptr[muxJT[(${databuf_str.replace("{}", inputMap(0).toString)} & ${mask}) + ${offset}]];\n")
          } else {
            sb.append(s"${outbuf_str.replace("{}", outputbufIdx.toString)} = databuf_ptr[muxJT[(${databuf_str.replace("{}", inputMap(0).toString)} & ${mask}) + ${offset}]];\n")
          }
        }
      }
      case 25 | 26 | 27 => {
        assert (skip == false, "Dynamic Shift operation should not skip output buffer writing.")
        sb.append("{\n")
        var dshift_string = if (varflag) "std::vector<uint64_t> dshift_r = {" else "std::vector<uint32_t> dshift_r = {"
        for (i <- 4 to inputMap.size-1) {
          if (i == 4) {
            dshift_string += (
              if (varflag)
                s"databuf_ptr_${inputMap(i)}"
              else
                s"${inputMap(i)}"
            )
          } else {
            dshift_string += (
              if (varflag)
                s", databuf_ptr_${inputMap(i)}"
              else
                s", ${inputMap(i)}"
            )
          }
        }
        dshift_string += "};\n"
        sb.append(dshift_string)
        if (varflag)
          sb.append("const uint64_t* __restrict rptr = dshift_r.data();\n")
        else
          sb.append("const uint32_t* __restrict rptr = dshift_r.data();\n")
        val length = circuitInput(inputMap(0)).toInt
        val outWidth = circuitInput(inputMap(3)).toInt
        val min_words = (outWidth + 63) / 64;
        if (varflag) {
          for (i <- 0 to min_words-1) {
            if (i == 0)
              sb.append(s"std::vector<uint64_t*> outbuf = {&outbuf_ptr_${outputbufIdx}")
            else
              sb.append(s", &outbuf_ptr_${outputbufIdx + i}")
          }
          sb.append("};\n")
          sb.append("uint64_t** __restrict outbuf_ptr = outbuf.data();\n")
          if (opType == 25)
            // dshl
            sb.append(s"dshl_operation(${length}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)}, ${outWidth}, rptr, outbuf_ptr, scratch1, scratch2);\n")
          else if (opType == 26)
            // dshr
            sb.append(s"dshr_operation(${length}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)}, ${outWidth}, rptr, outbuf_ptr, scratch1);\n")
          else if (opType == 27)
            // dshrS
            sb.append(s"dshrS_operation(${length}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)}, ${outWidth}, rptr, outbuf_ptr, scratch1);\n")
        } else {
          if (opType == 25)
            // dshl
            sb.append(s"dshl_operation(${length}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)}, ${outWidth}, rptr, databuf_ptr, outbuf_ptr, scratch1, scratch2, ${outputbufIdx});\n")
          else if (opType == 26)
            // dshr
            sb.append(s"dshr_operation(${length}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)}, ${outWidth}, rptr, databuf_ptr, outbuf_ptr, scratch1, ${outputbufIdx});\n")
          else if (opType == 27)
            // dshrS
            sb.append(s"dshrS_operation(${length}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${databuf_str.replace("{}", inputMap(2).toString)}, ${outWidth}, rptr, databuf_ptr, outbuf_ptr, scratch1, ${outputbufIdx});\n")
        }
        sb.append("}\n")

        outputbufIdx += min_words
      }
      case 28 => {
        // memw
        assert (skip == true, "Mem Write operation should skip output buffer writing.")
        val mem_flag = circuitInput(inputMap(2)).toInt
        val offset = circuitInput(inputMap(3)).toInt
        val mask = circuitInput(inputMap(4)).toInt
        sb.append(s"memw_operation(${databuf_str.replace("{}", inputMap(0).toString)}, ${databuf_str.replace("{}", inputMap(1).toString)}, ${mem_flag}, ${offset}, ${databuf_str.replace("{}", inputMap(5).toString)}, ${mask}, ${databuf_str.replace("{}", inputMap(6).toString)}, memory_64_ptr, memory_8_ptr);\n")
      }
      case _ =>
        throw new Exception(s"Unknown operation type: $opType")
    }
    if (skip == false && opType != 25 && opType != 26 && opType != 27) {
      outputbufIdx += 1
    }
    return outputbufIdx
  }

  def SU_gen(isnMap: ISNMap, nodeIDOrderMap: teAALIDHashMap, circuitInput: ArrayBuffer[BigInt], outputDir: String, tName: String): Unit = {
    val SUYML = new File(outputDir, s"kernel_SU_${tName}.cc")
    val SUwriter = new FileWriter(SUYML)
    val SUstrings = new StringBuilder()

    // traverse nodeIDOrderMap (HashMap[Int,HashMap[Int, HashMap[Int, Int]]])
    // layerNum, output index, input order, input index
    for ((layerNum, nodeMap) <- nodeIDOrderMap.toSeq.sortBy(_._1)) {
      SUstrings.append(s"// layer $layerNum\n")
      SUstrings.append("{\n")
      val (sortedOutMap, remainingMap) = sortOutMap(nodeMap, isnMap(layerNum))
      println(s"Layer $layerNum: total eliminated output buf count = ${sortedOutMap.size} (among ${nodeMap.size})")
      var layerOutCount = 0
      // schedule un-skippable op
      for (outputIdx <- remainingMap.toSeq.sorted) {
        val opType = isnMap(layerNum)(outputIdx)
        val inputMap = nodeMap(outputIdx)
        layerOutCount = SU_Build(SUstrings, false, false, opType, outputIdx, circuitInput, inputMap, layerOutCount)
      }
      // schedule skippable op first
      for ((_, outputIdx) <- sortedOutMap.toSeq.sortBy(_._1)) {
        // check operation type
        val opType = isnMap(layerNum)(outputIdx)
        val inputMap = nodeMap(outputIdx)
        layerOutCount = SU_Build(SUstrings, true, false, opType, outputIdx, circuitInput, inputMap, layerOutCount)
        // assert (layerOutCount == 0, "Layer output count should be zero at phase of skippable output buffer.")
      }
      var outbufIdx = 0
      for (outputIdx <- remainingMap.toSeq.sorted) {
        val opType = isnMap(layerNum)(outputIdx)
        val inputMap = nodeMap(outputIdx)
        if (opType == opIndex("dshl") || opType == opIndex("dshr") || opType == opIndex("dshrS")) {
          // dynamic shift
          val min_words = ((circuitInput(inputMap(3)) + 63) / 64).toInt;
          for (i <- 0 to min_words-1) {
            SUstrings.append(s"databuf_ptr[${outputIdx+i}] = outbuf_ptr[${outbufIdx}];\n")
            outbufIdx += 1
          }
        } else {
          SUstrings.append(s"databuf_ptr[${outputIdx}] = outbuf_ptr[${outbufIdx}];\n")
          outbufIdx += 1
        }
      }
      SUstrings.append("}\n")
    }

    val SUlines = Files.readAllLines(Paths.get(outputDir, "kernel_SU.cc")).asScala
    val SUnewLines = SUlines.map { line =>
      if (line.contains("// INSERT_INNER_DIMS_HERE"))
        SUstrings.toString()
      else
        line
    }

    SUwriter.write(SUnewLines.mkString("\n"))
    SUwriter.close()
  }

  def TI_gen(isnMap: ISNMap, nodeIDOrderMap: teAALIDHashMap, circuitInput: ArrayBuffer[BigInt], outputDir: String, tName: String): Unit = {
    val ju = new TeAALUtil()
    val TIYML = new File(outputDir, s"kernel_TI_${tName}.cc")
    val TIwriter = new FileWriter(TIYML)
    val TIstrings = new StringBuilder()
    val TIDeclarestrings = new StringBuilder()

    var max_outbuf_idx = 0

    val dtm_map = ju.readJsonMapUjson(s"${outputDir}/json/dtmSignal_${tName}.json")
    val reg_map = ju.readJsonMapArray(s"${outputDir}/json/InputRegMap_${tName}.json")
    TIstrings.append(s"io_success = databuf_ptr_${dtm_map("SimDTM$$inst.exit")} == 1;\n")
    TIstrings.append(s"databuf_ptr_${reg_map("reset")(0)} = done_reset ? 0 : 1;\n")
    reg_map.clear()

    // traverse nodeIDOrderMap (HashMap[Int,HashMap[Int, HashMap[Int, Int]]])
    // layerNum, output index, input order, input index
    for ((layerNum, nodeMap) <- nodeIDOrderMap.toSeq.sortBy(_._1)) {
      TIstrings.append(s"// layer $layerNum\n")
      TIstrings.append("{\n")
      val (sortedOutMap, remainingMap) = sortOutMap(nodeMap, isnMap(layerNum))
      var layerOutCount = 0
      // schedule un-skippable op
      for (outputIdx <- remainingMap.toSeq.sorted) {
        val opType = isnMap(layerNum)(outputIdx)
        val inputMap = nodeMap(outputIdx)
        layerOutCount = SU_Build(TIstrings, false, true, opType, outputIdx, circuitInput, inputMap, layerOutCount)
      }
      // schedule skippable op first
      for ((_, outputIdx) <- sortedOutMap.toSeq.sortBy(_._1)) {
        // check operation type
        val opType = isnMap(layerNum)(outputIdx)
        val inputMap = nodeMap(outputIdx)
        layerOutCount = SU_Build(TIstrings, true, true, opType, outputIdx, circuitInput, inputMap, layerOutCount)
      }
      var outbufIdx = 0
      for (outputIdx <- remainingMap.toSeq.sorted) {
        val opType = isnMap(layerNum)(outputIdx)
        val inputMap = nodeMap(outputIdx)
        if (opType == opIndex("dshl") || opType == opIndex("dshr") || opType == opIndex("dshrS")) {
          // dynamic shift
          val min_words = ((circuitInput(inputMap(3)) + 63) / 64).toInt;
          for (i <- 0 to min_words-1) {
            TIstrings.append(s"databuf_ptr_${outputIdx+i} = outbuf_ptr_${outbufIdx};\n")
            outbufIdx += 1
          }
        } else {
          TIstrings.append(s"databuf_ptr_${outputIdx} = outbuf_ptr_${outbufIdx};\n")
          outbufIdx += 1
        }
      }
      if (outbufIdx > max_outbuf_idx) {
        max_outbuf_idx = outbufIdx
      }
      TIstrings.append("}\n")
    }

    // DTM related
    TIstrings.append("if (done_reset) {\n")
    TIstrings.append("    dtm_t::resp resp_bits;\n")
    TIstrings.append(s"    resp_bits.resp = static_cast<uint32_t>(databuf_ptr_${dtm_map("SimDTM$$inst.debug_resp_bits_resp")});\n")
    TIstrings.append(s"    resp_bits.data = static_cast<uint32_t>(databuf_ptr_${dtm_map("SimDTM$$inst.debug_resp_bits_data")});\n")
    TIstrings.append(s"    dtm->tick(databuf_ptr_${dtm_map("SimDTM$$inst.debug_req_ready")} != 0,\n")
    TIstrings.append(s"                databuf_ptr_${dtm_map("SimDTM$$inst.debug_resp_valid")} != 0,\n")
    TIstrings.append("                resp_bits);\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.debug_resp_ready")} = 1;\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.debug_req_valid")} = dtm->req_valid() ? 1 : 0;\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.debug_req_bits_addr")} = static_cast<uint64_t>(dtm->req_bits().addr);\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.debug_req_bits_op")} = static_cast<uint64_t>(dtm->req_bits().op);\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.debug_req_bits_data")} = static_cast<uint64_t>(dtm->req_bits().data);\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.exit")} = static_cast<uint64_t>(dtm->done() ? (dtm->exit_code() << 1 | 1) : 0);\n")
    TIstrings.append("} else {\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.debug_req_valid")} = 0;\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.debug_resp_ready")} = 0;\n")
    TIstrings.append(s"    databuf_ptr_${dtm_map("SimDTM$$inst.exit")} = 0;\n")
    TIstrings.append("}\n")

    // Declare input databuf_ptr
    for (i <- 0 to circuitInput.size-1) {
      TIDeclarestrings.append(s"    uint64_t databuf_ptr_${i} = ${circuitInput(i)};\n")
    }
    for (i <- 0 to max_outbuf_idx-1) {
      TIDeclarestrings.append(s"    uint64_t outbuf_ptr_${i} = 0;\n")
    }
    // load muxJT
    val muxJT = ju.readJumpTableSet(s"${outputDir}/txt/muxJT_${tName}.txt")
    TIDeclarestrings.append(s"    std::unordered_map<uint32_t, uint64_t*> data_mux_map = {")
    for (i <- muxJT) {
      TIDeclarestrings.append(s"{${i}, &databuf_ptr_${i}},\n")
    }
    TIDeclarestrings.append("};\n")

    val TIlines = Files.readAllLines(Paths.get(outputDir, "kernel_TI.cc")).asScala
    val TInewLines = TIlines.map { line =>
      if (line.contains("// INSERT_INNER_DIMS_HERE"))
        TIstrings.toString()
      else if (line.contains("// INSERT_DECLARE_HERE"))
        TIDeclarestrings.toString()
      else
        line
    }

    TIwriter.write(TInewLines.mkString("\n"))
    TIwriter.close()
  }

  def IU_gen(isnMap: ISNMap, nodeIDOrderMap: teAALIDHashMap, outputDir: String, tName: String): Unit = {
    // inline functions for IU kernel
    val n_loop_map = HashMap[Int, String]()
    n_loop_map(0) = "add_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(1) = "sub_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(2) = "mdr_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(3) = "asSInt_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(4) = "lt_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(5) = "ltS_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(6) = "leq_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(7) = "leqS_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(8) = "eq_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(9) = "neq_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(10) = "shl_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(11) = "shr_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(12) = "shrS_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(13) = "and_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(14) = "or_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(15) = "xor_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(16) = "xorr_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(17) = "cat_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(18) = "bits_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(19) = "mux_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(20) = "assign_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(21) = "memr_loop(rptr, databuf_ptr, outbuf_ptr, memory_64_ptr, memory_8_ptr, start, end);"
    n_loop_map(22) = "orchain_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(23) = "xorchain_loop(rptr, databuf_ptr, outbuf_ptr, start, end);"
    n_loop_map(24) = "muxchain_loop(rptr, databuf_ptr, outbuf_ptr, muxJT_ptr, start, end);"
    n_loop_map(25) = "out_count = dshl_loop(rptr, databuf_ptr, outbuf_ptr, scratch1, scratch2, start, out_count, end);"
    n_loop_map(26) = "out_count = dshr_loop(rptr, databuf_ptr, outbuf_ptr, scratch1, start, out_count, end);"
    n_loop_map(27) = "out_count = dshrS_loop(rptr, databuf_ptr, outbuf_ptr, scratch1, start, out_count, end);"
    n_loop_map(28) = "memw_loop(rptr, databuf_ptr, outbuf_ptr, memory_64_ptr, memory_8_ptr, start, end);"
    val IUYML = new File(outputDir, s"kernel_IU_${tName}.cc")
    val IUwriter = new FileWriter(IUYML)
    val IUstrings = new StringBuilder()

    val sortedOuter: SortedMap[Int, Map[Int, Int]] = SortedMap.from(
      isnMap.map { case (k, innerMap) => {
          // println("Layer ", k, " ops: ", innerMap.values.toSeq)
          k -> innerMap.toMap  // Convert inner mutable HashMap to immutable Map
        }
      }
    )
    val valueCountsPerOuterKey: SortedMap[Int, SortedMap[Int, Int]] = SortedMap.from {
      sortedOuter.map { case (outerKey, innerMap) =>
        val valueCounts = innerMap.values
          .groupBy(identity)
          .view
          .mapValues(_.size)
          .toMap

        // Sort valueCounts by value (i.e., make it a SortedMap)
        (outerKey, SortedMap.from(valueCounts))
      }
    }

    valueCountsPerOuterKey.foreach { case (outerKey, counts) => {
      // println(s"Outer key $outerKey:")
      IUstrings.append(s"// layer $outerKey\n")
      IUstrings.append("{\n")
      var layer_op_counter = 0
      var layer_op_total = 0
      var out_flag = false
      counts.foreach { case (value, count) =>
        // println(s"  Value $value occurs $count times")
        val loopBody = n_loop_map(value)
        layer_op_total += count

        if (value != 25 && value != 26 && value != 27) {
          IUstrings.append(loopBody.replace("start", s"${layer_op_counter}").replace("end", s"${layer_op_counter+count}"))
          IUstrings.append("\n")
          if (value != 28)
            layer_op_counter += count
        } else {
          out_flag = true
          val matchingKeys: HashSet[Int] = HashSet.from(
            isnMap(outerKey).collect { case (k, v) if v == value => k }
          )
          val shift_count = shiftBlockCount(outerKey, matchingKeys, nodeIDOrderMap)
          if (counts.contains(25)) {
            if (value == 25) {
              IUstrings.append(s"out_count = ${layer_op_counter};\n")
              IUstrings.append(loopBody.replace("start", s"${layer_op_counter}").replace("end", s"${layer_op_counter+shift_count}"))
            } else {
              IUstrings.append(loopBody.replace("start", s"${layer_op_counter}").replace("end", s"${layer_op_counter+shift_count}"))
            }
          } else if (counts.contains(26)) {
            if (value == 26) {
              IUstrings.append(s"out_count = ${layer_op_counter};\n")
              IUstrings.append(loopBody.replace("start", s"${layer_op_counter}").replace("end", s"${layer_op_counter+shift_count}"))
            } else {
              IUstrings.append(loopBody.replace("start", s"${layer_op_counter}").replace("end", s"${layer_op_counter+shift_count}"))
            }
          } else if (counts.contains(27)) {
            if (value == 27) {
              IUstrings.append(s"out_count = ${layer_op_counter};\n")
              IUstrings.append(loopBody.replace("start", s"${layer_op_counter}").replace("end", s"${layer_op_counter+shift_count}"))
            } else {
              IUstrings.append(loopBody.replace("start", s"${layer_op_counter}").replace("end", s"${layer_op_counter+shift_count}"))
            }
          }
          IUstrings.append("\n")
          layer_op_counter += shift_count
        }
      }

      IUstrings.append("size_t j = 0;\n")
      if (out_flag) {
        IUstrings.append(s"for (; j + 23 < out_count; j += 24) {\n")
      } else {
        IUstrings.append(s"for (; j + 23 < ${layer_op_counter}; j += 24) {\n")
      }
      IUstrings.append("    COPY24();\n")
      IUstrings.append("}\n\n")
      if (out_flag) {
        IUstrings.append(s"for (; j < out_count; ++j) {\n")
      } else {
        IUstrings.append(s"for (; j < ${layer_op_counter}; ++j) {\n")
      }
      IUstrings.append("    databuf_ptr[*s_ptr++] = outbuf_ptr[j];\n")
      IUstrings.append("}\n")
      IUstrings.append("}\n\n")
    }}

    val IUlines = Files.readAllLines(Paths.get(outputDir, "kernel_IU.cc")).asScala
    val IUnewLines = IUlines.map { line =>
      if (line.contains("// INSERT_INNER_DIMS_HERE"))
        IUstrings.toString()
      else
        line
    }

    IUwriter.write(IUnewLines.mkString("\n"))
    IUwriter.close()
  }
}


object TeAALTranspiler {
  type convHashMap = HashMap[String, HashMap[String, Int]]
  type ISNMap = HashMap[Int,HashMap[Int, Int]]
  type teAALIDHashMap = HashMap[Int,HashMap[Int, HashMap[Int, Int]]]
}