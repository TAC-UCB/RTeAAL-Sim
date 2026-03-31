package essent

import org.yaml.snakeyaml.Yaml
import scala.jdk.CollectionConverters._
import scala.collection.mutable.{Map => MMap}
import scala.io.Source

import upickle.default._
import java.io.{File, FileWriter, Writer, PrintWriter}

import essent.StatementGraph._
import essent.TeAALExtractor._

import essent.ir._
import firrtl._
import firrtl.ir._

import firrtl.annotations._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.Utils._

import collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue, Map, LinkedHashMap}
import scala.collection.immutable.{SortedMap, TreeMap}

import essent.Extract._
import scala.util.Random

class TeAALDFGraphExtractor(opInBufferOrder: opInOrder, inEdgeBuffer: BufferHashMap, outEdgeBuffer: BufferHashMap, oEBufferR: HashMap[String, String], 
                  bitWidthMetadata: HashMap[String, Int], regOrder: HashSet[String], regNextOrder: HashSet[String],
                  muxJumpTable: ArrayBuffer[String], dshiftGroup: HashSet[ArrayBuffer[String]], mem_extra_inputs: HashMap[String, HashSet[Int]], randomChain_Inputs: HashMap[String, Int]) {

  import TeAALDFGraphExtractor.{IOpArray, teAALHashMap, teAALIDHashMap, convHashMap, ISNMap}
  import TeAALExtractor.{BufferHashMap, opInOrder}

  val ju = new TeAALUtil()

  var inNeigh: BufferHashMap = HashMap[String, HashSet[String]]()
  var outNeigh: BufferHashMap = HashMap[String, HashSet[String]]()
  var nodeToLayer = HashMap[String, Int]()
  var iEBufferR = new BufferHashMap()
  var layerMap: BufferHashMap = HashMap[String, HashSet[String]]()
  var dtmSignalCollect = HashMap[String, Int]()
  var debugMap =  HashMap[Int, HashMap[Int, String]]()

  var newregorder = new HashMap[String, Int]()
  var regIOP = HashMap[String, Int]()  // reg and the last layer it is used
  var regUpOpReg = HashMap[String, String]() // op that produce regNext and its corresponding reg

  var TOTAL_IOP = 0
  var LAYER_NUM = 0
  var maxL = 0
  var iopNode = HashMap[String, Int]()

  var input_idx_map = HashMap[String, ArrayBuffer[Int]]()

  var nodeIDOrderMap = new teAALIDHashMap()
  var isnMap = new ISNMap()
  var layerNodeID = new BufferHashMap()
  var layerNodeIDN = new convHashMap()
  var circuitInput = new ArrayBuffer[BigInt]()
  var opIndex = HashMap[String, Int]()
  var dshiftOrder = HashMap[Int, HashMap[Int, HashSet[Int]]]() // layerNum->(first idx->(rest idx))
  var longbitsOrder = HashMap[Int, HashMap[Int, HashSet[Int]]]()
  var muxJT_idx = ArrayBuffer[BigInt]()

  def reg_update(): HashMap[String, HashSet[Int]] = {
    var reg_extra_inputs = HashMap[String, HashSet[Int]]()
    opInBufferOrder foreach {case(op, inputs)=>{
      if (regNextOrder.contains(oEBufferR(op))) {
        if (!oEBufferR(op).contains("@next")) {
          println("reg up: ", op, inputs, oEBufferR(op))
        }
        val reg_up_name = oEBufferR(op)
        val old_max_idx = inputs.values.flatMap(_.toSet).max
        val reg_orig_name = reg_up_name.replace("@next", "")
        val reg_input_ops = inEdgeBuffer.getOrElse(reg_orig_name, HashSet[String]())
        if (reg_input_ops.nonEmpty) {
          var idx = old_max_idx + 1
          var idx_arr = HashSet[Int]()
          reg_input_ops foreach {ops=>{
            if (ops != op) {
              if (!inputs.contains(oEBufferR(ops))) {
                inputs.getOrElseUpdate(oEBufferR(ops), HashSet.empty) += idx
                idx_arr += idx
                inEdgeBuffer.getOrElseUpdate(oEBufferR(ops), HashSet.empty) += op
                idx += 1
              }
            }
          }}
          if (idx_arr.nonEmpty) {
            reg_extra_inputs(op) = idx_arr
          }
        }
      }
    }}
    return reg_extra_inputs
  }

  def opGraphConstructor(iEBuffer: BufferHashMap, oEBuffer: BufferHashMap): Unit = {
    iEBuffer foreach { kval =>
      if (oEBuffer.contains(kval._1)) {
        oEBuffer(kval._1) foreach { op =>
          if (!outNeigh.contains(op)) 
            outNeigh += op->kval._2.clone()
          else
            outNeigh(op) ++= kval._2.clone()
        }
        kval._2 foreach { op => {
          if (!inNeigh.contains(op))
            inNeigh += op->oEBuffer(kval._1).clone()
          else
            inNeigh(op) ++= oEBuffer(kval._1).clone()
          
          opInBufferOrder(op)(kval._1).clone() foreach {idx=>{
          opInBufferOrder(op).getOrElseUpdate(oEBuffer(kval._1).head, HashSet.empty) += idx
        }}

          opInBufferOrder(op).remove(kval._1)
        }}
        iEBuffer.remove(kval._1)
        oEBuffer.remove(kval._1)
      }
    }
  }

  def nodeRemover(inNeigh: BufferHashMap, node: String, outNeigh: BufferHashMap): Unit = {
    outNeigh.get(node).foreach { outNodes =>
      outNodes.foreach { kv =>
        inNeigh.get(kv).foreach(_.remove(node))
    }}

    if (!outNeigh.contains(node)) {
      inNeigh.foreach { case (key, neighbors) =>
        if (neighbors.contains(node)) {
          inNeigh(key).remove(node)
        }
      }
    }
  }

  def iOPAdder(node: String, preLayer: String, layerCounter: Int): Unit = {
    if (inNeigh.contains(node)) {
      inNeigh(node) foreach {neighbor=>{
        if (!layerMap(preLayer).contains(neighbor)) {
          for (i <- nodeToLayer(neighbor)+1 until layerCounter) {
            layerMap(s"layer$i") += neighbor
            TOTAL_IOP = TOTAL_IOP + 1
          }

          if (regOrder.contains(neighbor))
            regIOP += neighbor->(layerCounter-1)
        }
      }}
    }
    if (iEBufferR.contains(node)) {
      iEBufferR(node) foreach {ins=>{
        if (!layerMap(preLayer).contains(ins)) {
          for (i <- 0 until layerCounter) {
            layerMap(s"layer$i") += ins
            TOTAL_IOP = TOTAL_IOP + 1
          }
          // do not need to check the max, because we are iterating in an asceding layer order
          if (regOrder.contains(ins))
            regIOP += ins->(layerCounter-1)
        }
      }}
    }
  }

  def layerExtract(outNeigh_cp: BufferHashMap, inNeigh_cp: BufferHashMap, iEBuffer: BufferHashMap, oEBuffer: BufferHashMap): Unit = {
    layerMap += "layer0"->HashSet[String]()
    layerMap += "input"->HashSet[String]()

    iEBuffer.foreach { case (key, ops) =>
      layerMap("input") += key
      ops.foreach { op =>
        if (!inNeigh_cp.contains(op)) {
          layerMap("layer0") += op
          nodeToLayer += op->0
          nodeRemover(inNeigh_cp, op, outNeigh_cp)
        }
      }
    }

    regOrder foreach {reg=>{
      if (!(layerMap("input")).contains(reg)) {
        layerMap("input") += reg
      }
    }}

    var layerCounter = 1
    var rmNode = HashSet[String]()

    while (inNeigh_cp.nonEmpty) {
      val currentLayer = s"layer$layerCounter"
      val preLayer = s"layer${layerCounter-1}"
      layerMap += currentLayer -> HashSet()
      inNeigh_cp.foreach { case (node, neighbors) =>
        if (neighbors.isEmpty) {
          layerMap(currentLayer) += node
          iopNode(node) = layerCounter
          nodeToLayer += node -> layerCounter
          inNeigh_cp.remove(node)
          rmNode += node
        }
      }

      if (rmNode.isEmpty) {
        println(inNeigh_cp)
        throw new Exception(s"No nodes are removed for layer $layerCounter")
      }

      rmNode.foreach { node =>
        nodeRemover(inNeigh_cp, node, outNeigh_cp)
      }
      layerCounter = layerCounter + 1
      rmNode.clear()
    }
    if (inNeigh_cp.nonEmpty) throw new Exception("Some nodes are left after layer extraction!")
    LAYER_NUM = layerCounter
  }
  
  def rvmExtraMemInput(reg_extra_inputs: HashMap[String, HashSet[Int]]): Unit = {
    val total_extra_input = reg_extra_inputs ++ mem_extra_inputs
    total_extra_input foreach {case(memw_op, inputs)=>{
      val total_ins = opInBufferOrder(memw_op)
      val in_ops = inNeigh(memw_op)
      inputs foreach {input=>{
        val in_key = ju.inputSearch(total_ins, input)
        if (total_ins.contains(in_key)) {
          total_ins(in_key) -= input
          if (total_ins(in_key).isEmpty) {
            total_ins.remove(in_key)
            if (in_ops.contains(in_key)) {
              in_ops.remove(in_key)
            }
            if (outNeigh(in_key).contains(memw_op)) {
              outNeigh(in_key).remove(memw_op)
            }
            if (inEdgeBuffer.contains(in_key)) {
              inEdgeBuffer(in_key) -= memw_op
              if (inEdgeBuffer(in_key).isEmpty) {
                inEdgeBuffer.remove(in_key)
              }
            }
          }
        }
      }}
    }}

    opInBufferOrder foreach {case(op, input_info)=>{
      if (op.startsWith("memw") || op.startsWith("stop")) {
        val tmp_output = oEBufferR(op)
        outEdgeBuffer.remove(tmp_output)
        
        oEBufferR.remove(op)
        if (inEdgeBuffer.contains(tmp_output)) {
          val ops = inEdgeBuffer(tmp_output)
          ops foreach {need_to_change_op=>{
            val need_to_change_op_inputs = opInBufferOrder(need_to_change_op)
            need_to_change_op_inputs -= tmp_output
          }}
        }
        inEdgeBuffer -= tmp_output
      }
    }}
  }

  def addAllIOP(outEdgeBuffer: BufferHashMap): Unit = {
    iopNode foreach {case(node, layercount)=>{
      val preLayer = s"layer${layercount-1}"
      iOPAdder(node, preLayer, layercount)
    }}

    outEdgeBuffer.foreach { case (key, ops) => {

      if (regNextOrder.contains(key)) {
        ops foreach {op=>{
          val oreg = key.replace("@next", "")
          regUpOpReg += op->oreg
          val nextL = nodeToLayer(op)

          if (regIOP.contains(oreg)) {
            if (nextL <= regIOP(oreg)) {
              nodeToLayer(op) = regIOP(oreg)+1
              layerMap(s"layer${nextL}") -= op
              layerMap.getOrElseUpdate(s"layer${regIOP(oreg)+1}", HashSet.empty) += op
              if (regIOP(oreg)+1 > LAYER_NUM) LAYER_NUM = regIOP(oreg)+1

              val inputs = HashSet() ++ opInBufferOrder(op).keys
              for (i <- nextL to regIOP(oreg)) {
                inputs foreach {input=>{
                  layerMap(s"layer$i") += input
                  TOTAL_IOP = TOTAL_IOP + 1
                }}
              }
            }
          } else {
            // no iop added for that reg, meaning al its ops are done in layer 0
          }
        }}
      }
    }}
  }

  def opIndexAssign(): Unit = {
    opIndex += "add"->0
    opIndex += "sub"->1
    opIndex += "mul"->2
    opIndex += "mulS"->2
    opIndex += "div"->2
    opIndex += "divS"->2
    opIndex += "rem"->2
    opIndex += "asSInt"->3
    opIndex += "lt"->4
    opIndex += "ltS"->5
    opIndex += "leq"->6
    opIndex += "leqS"->7
    opIndex += "eq"->8
    opIndex += "neq"->9
    opIndex += "shl"->10
    opIndex += "shr"->11
    opIndex += "shrS"->12
    opIndex += "and"->13
    opIndex += "or"->14
    opIndex += "xor"->15
    opIndex += "xorr"->16
    opIndex += "cat"->17
    opIndex += "bits"->18
    opIndex += "mux"->19
    opIndex += "assign"->20
    opIndex += "memr"->21
    opIndex += "orchain"->22
    opIndex += "xorchain"->23
    opIndex += "chain"->24
    opIndex += "dshl"->25
    opIndex += "dshr"->26
    opIndex += "dshrS"->27
    opIndex += "memw"->28
  }

  def paramsExtract(): Unit = {
    for (i <- 0 to layerNodeIDN.size-2) {

      isnMap += i->HashMap[Int, Int]()

      layerNodeIDN("layer"+i.toString) foreach {ops=>{
        val opN = ops._1.split("_\\+_")(0)
        if (!opIndex.contains(opN) && !regOrder.contains(opN) && !regNextOrder.contains(opN)) {println("extra opN", opN, ops._1)}

        if (opIndex.contains(opN)) {
          var opVal = opIndex(opN)
          isnMap(i) += ops._2->opVal
        }
        else if (i == layerNodeIDN.size-2) {
          isnMap(i) += ops._2->opIndex("assign")
        }
        else {
          isnMap(i) += ops._2->opIndex("assign")
        }
      }}
    }
  }

  def getMaxL(): Unit = {
    layerMap foreach (layer=>{
      maxL = maxL max layer._2.size
    })
    muxJumpTable foreach {jump_idx=>{
      if (ju.isInte(jump_idx) && !layerMap("input").contains(jump_idx)) {
        maxL = maxL + 1
      }
    }}
    maxL = maxL+layerMap("input").size
  }

  def iEBufferReverse(iEBuffer: BufferHashMap, iEBufferR: BufferHashMap): Unit = {
    iEBuffer foreach {kv => {
      kv._2 foreach {v => {
        if (iEBufferR.contains(v)) {
          iEBufferR(v) += kv._1
        }
        else {
          iEBufferR += v->HashSet[String]()
          iEBufferR(v) += kv._1
        }
      }}
    }}
  }


  def nodeIDOrderMapUpdate(nodeOrderMap: teAALHashMap, preLayerNum: Int, preLayerName: String, prepreLayerName: String): Unit = {
    if (!nodeIDOrderMap.contains(preLayerNum)) {
      nodeIDOrderMap += preLayerNum->HashMap[Int, HashMap[Int, Int]]()
    }

    nodeOrderMap(preLayerName) foreach { valin => {
      if (layerNodeIDN(preLayerName).contains(valin._1)) {
        nodeIDOrderMap(preLayerNum) += layerNodeIDN(preLayerName)(valin._1)->HashMap[Int, Int]()
        valin._2 foreach { ins => {
          if (layerNodeIDN(prepreLayerName).contains(ins)) {
            opInBufferOrder (valin._1)(ins) foreach { idx => {
              nodeIDOrderMap(preLayerNum)(layerNodeIDN(preLayerName)(valin._1)) += idx->layerNodeIDN(prepreLayerName)(ins)
            }}
          }
          else if (layerNodeIDN("input").contains(ins)) {
            opInBufferOrder (valin._1)(ins) foreach { idx => {
              nodeIDOrderMap(preLayerNum)(layerNodeIDN(preLayerName)(valin._1)) += idx->layerNodeIDN("input")(ins)
            }}
          }
          else if (nodeToLayer.contains(ins)) {
            opInBufferOrder(valin._1)(ins) foreach { idx => {
              nodeIDOrderMap(preLayerNum)(layerNodeIDN(preLayerName)(valin._1)) += idx->layerNodeIDN(s"layer${nodeToLayer(ins)}")(ins)
            }}
          }
          else {
            // println("nodeIDOrderMapUpdate: ", preLayerName, valin._1, ins)
          }
      }}
      } 
    }}
    nodeOrderMap.remove(preLayerName)
  }

  def dshiftCheck(op: String): Boolean = {
    dshiftGroup foreach {group=>{
      if (group.contains(op)) {
        return true
      }
    }}
    return false
  }

  def dshiftExtract(op: String): ArrayBuffer[String] = {
    dshiftGroup.find(_.contains(op)).getOrElse(ArrayBuffer.empty)
  }

  def findContiguousSet(set: HashSet[Int], length: Int): Option[Seq[Int]] = {
    val sorted = set.toArray.sorted
    for (i <- 0 to sorted.length - length) {
      val window = sorted.slice(i, i + length)
      if (window.last - window.head == length - 1 && window.sliding(2).forall { case Array(a, b) => b == a + 1 }) {
        return Some(window)
      }
    }
    None
  }

  def makeIncreasingBuffer(maxL: Int, size: Int): Seq[Int] = {
    Seq.tabulate(size)(i => maxL + i)
  }

  def opTypeClass(ops: HashSet[String]): SortedMap[Int, HashSet[String]] = {
    var opTypeMap = SortedMap[Int, HashSet[String]]()
    ops foreach {op=>{
      val opN = if (op.contains("_+_")) op.split("_\\+_")(0) else op
      val opIdx = if (opIndex.contains(opN)) opIndex(opN) else opIndex("assign")
      if (opTypeMap.contains(opIdx)) {
        opTypeMap(opIdx) += op
      } else {
        opTypeMap += opIdx->HashSet(op)
      }
    }}
    opTypeMap
  }

  def orderAssign(iEBuffer: BufferHashMap, oEBuffer: BufferHashMap, iNeigh: BufferHashMap, nodeToLayer: HashMap[String, Int], opInBufferOrder : opInOrder): Unit = {
    var freeIdx = new HashSet[Int]()
    var memsave = new HashMap[String, Int]()
    var memsave2 = new HashMap[String, Int]()
    var flag = true
    var reduceop = new HashSet[String]()
    var nodeOrderMap = new teAALHashMap()
    var layerIn = new HashMap[String, Int]()
    var inc = 0
    var dshiftUsed = HashSet[String]()
    var longbitsUsed = HashSet[String]()

    var actual_dtmin = 0

    val SimDTM_input = ArrayBuffer("SimDTM$$inst.debug_resp_ready", "SimDTM$$inst.debug_req_valid", "SimDTM$$inst.debug_req_bits_addr", "SimDTM$$inst.debug_req_bits_op", "SimDTM$$inst.debug_req_bits_data", "SimDTM$$inst.exit")
    val SimDTM_output: ArrayBuffer[String] =
      ArrayBuffer.from(
        Source.fromFile("preserved_signals.txt")
          .getLines()
          .map(_.trim)
          .filter(_.nonEmpty)
      )

    SimDTM_input foreach {SimDTM_in => {
      if (inEdgeBuffer.contains(SimDTM_in) && bitWidthMetadata.contains(SimDTM_in)) {
        layerIn += SimDTM_in->inc
        dtmSignalCollect(SimDTM_in) = inc
        var bw = bitWidthMetadata(SimDTM_in)
        circuitInput += ju.randValue(bw)
        inc += 1
        actual_dtmin += 1
      }
    }}

    for (placeholder<-0 until SimDTM_output.size) {
      circuitInput += BigInt(0)
      inc += 1
    }

    var current_layer = opTypeClass(layerMap(s"layer0"))
    var current_layer_order = ArrayBuffer[String]()
    var input_vars = ArrayBuffer[String]()

    current_layer foreach { case (opType, ops) => {
      ops foreach { op => {
        if (!op.contains("stop") && !op.contains("memw") && oEBufferR.contains(op) && SimDTM_output.contains(oEBufferR(op))) {
          val simdtm_out = actual_dtmin + SimDTM_output.indexOf(oEBufferR(op))
          current_layer_order += op
        } else {
          current_layer_order += op
          if (layerMap(s"input").contains(op)) {
          }
          else if (opInBufferOrder.contains(op)) {
            val sortedKeys = opInBufferOrder(op).toSeq.sortBy { case (_, hs) => hs.min }.map(_._1)
            sortedKeys foreach {input_var=>
              if (!input_vars.contains(input_var)) {
                input_vars += input_var
              }
            }
          }
        }
      }}
    }}

    // println(current_layer)
    // println("current_layer_order: ", current_layer_order)

    val remain_in = layerMap("input").toSet -- input_vars.toSet
    val sorted_input = input_vars ++ remain_in.toSeq.sorted // sort by alphebet

    sorted_input foreach {op=>{
      if (!SimDTM_input.contains(op)) {
        layerIn += op->inc

        if (ju.isInte(op)) {
          circuitInput += ju.hexInt(op)
        } else if (ju.isDecimalInt(op)) {
          circuitInput += BigInt(op)
        } else {
          var bw = bitWidthMetadata(op)
          if (bw > 64) {
            bw = 64
          }
          circuitInput += ju.randValue(bw)
          // circuitInput += BigInt(0)
          if (!outEdgeBuffer.contains(op) || regOrder.contains(op)) {
            // val topIn = reversein(op)
            val topIn = if (op.contains("_+=+_")) op.split("_\\+=\\+_")(0) else op
            val topIN_idx = if (op.contains("_+=+_")) op.split("_\\+=\\+_")(1).toInt else 0
            if (!input_idx_map.contains(topIn)) {
              input_idx_map(topIn) = ArrayBuffer.fill(topIN_idx+1)(0)
            }
            if (input_idx_map.contains(topIn)) {
              if (input_idx_map(topIn).size < topIN_idx+1) {
                for (i <- input_idx_map(topIn).size to topIN_idx) {
                  input_idx_map(topIn) += 0
                }
                input_idx_map(topIn)(topIN_idx) = inc
              } else {
                input_idx_map(topIn)(topIN_idx) = inc
              }
            }
          }
          else {

          }
        }
        
        if (regOrder.contains(op)) {
          newregorder += op->inc
        }
        inc += 1
      }
    }}
    // println(s"newregorder: ${newregorder}")

    layerNodeIDN += "input"->layerIn.clone()
    muxJumpTable foreach {jump_idx=>{
      if (ju.isInte(jump_idx) && !layerNodeIDN("input").contains(jump_idx)) {
        layerNodeIDN("input") += jump_idx->inc
        inc += 1
        circuitInput += ju.hexInt(jump_idx)
      }
    }}

    // println("input inc", inc)

    for (i <- inc until maxL) {
      freeIdx += i
    }

    layerNodeIDN += "layer0"->HashMap[String, Int]()
    nodeOrderMap += "layer0"->HashMap[String, HashSet[String]]()

    var reduce_arr = ArrayBuffer[String]()

    current_layer_order foreach { op => {
      nodeOrderMap("layer0") += op->HashSet[String]()
      if (iEBufferR.contains(op)) {
        nodeOrderMap("layer0")(op) ++= iEBufferR(op).clone()
      } else if (sorted_input.contains(op)) {
        nodeOrderMap("layer0")(op) += op
      }

      val opIns = nodeOrderMap("layer0")(op)

      if (layerNodeIDN("input").contains(op)) {
        val last_op_pos = layerNodeIDN("input")(op)
        layerNodeIDN("layer0") += op->last_op_pos
        freeIdx -= last_op_pos
      } 
      else if (regUpOpReg.contains(op)) {
        layerNodeIDN("layer0") += op->newregorder(regUpOpReg(op))
        freeIdx -= newregorder(regUpOpReg(op))
      } 
      else if (!op.contains("stop") && !op.contains("memw") && SimDTM_output.contains(oEBufferR(op))) {
        val simdtm_out = actual_dtmin + SimDTM_output.indexOf(oEBufferR(op))
        layerNodeIDN("layer0") += op->simdtm_out
        dtmSignalCollect(oEBufferR(op)) = simdtm_out
        freeIdx -= simdtm_out
      }
      else {
        // reduceop += op
        reduce_arr += op
      }
    }}

    val sortedfreeIdx = freeIdx.toSeq.sorted
    var freeIdx_idx = 0

    reduce_arr foreach { op=> {
      if (!dshiftCheck(op) && !op.startsWith("longbits")) {
        if (!layerNodeIDN("layer0").contains(op)) {
          if (freeIdx_idx < sortedfreeIdx.size) {
            val cur_idx = sortedfreeIdx(freeIdx_idx)
            layerNodeIDN("layer0") += op->cur_idx
            freeIdx_idx += 1
            freeIdx -= cur_idx
            reduceop += op
          } else {
            maxL += 1
            val cur_idx = maxL
            layerNodeIDN("layer0") += op->cur_idx
            freeIdx_idx += 1
            freeIdx -= cur_idx
            reduceop += op
          }
        }
      }
    }}

    reduce_arr --= reduceop

    reduce_arr foreach {op=> {
      if (dshiftCheck(op)) {
        if (!dshiftUsed.contains(op)) {
          var innerMap = HashMap[Int, HashSet[Int]]()
          val group = dshiftExtract(op)
          dshiftUsed ++= group
          val idx_list = findContiguousSet(freeIdx, group.size)
          if (idx_list.isDefined) {
            for (id <- 0 until group.size) {
              layerNodeIDN("layer0") += group(id)->idx_list.get(id)
              freeIdx -= idx_list.get(id)
            }
            val key = idx_list.get.head
            val valueSet = HashSet(idx_list.get.tail: _*)
            innerMap(key) = valueSet

          } else {
            val increase_list = makeIncreasingBuffer(maxL, group.size)
            for (id <- 0 until group.size) {
              layerNodeIDN("layer0") += group(id)->increase_list(id)
              freeIdx -= increase_list(id)
            }
            val key = increase_list.head
            val valueSet = HashSet(increase_list.tail: _*)
            innerMap(key) = valueSet
            maxL += group.size
          }
          dshiftOrder.getOrElseUpdate(0, HashMap.empty) ++= innerMap
        }
      } else {
        assert(false)
      }
    }}

    // println(reduce_arr)

    reduceop.clear()
    reduce_arr.clear()
    current_layer_order.clear()
    freeIdx.clear()

    if (layerMap.size == 2) {
      layerNodeIDN("input") foreach {node=>{
        if (layerNodeIDN("layer0").contains(node._1)) {
          layerNodeIDN("layer0").remove(node._1)
        }
      }}

      nodeIDOrderMap += 0->HashMap[Int, HashMap[Int, Int]]()
      nodeOrderMap("layer0") foreach { valin => {
        if (layerNodeIDN("layer0").contains(valin._1)) {
          nodeIDOrderMap(0) += layerNodeIDN("layer0")(valin._1)->HashMap[Int, Int]()
          valin._2 foreach { ins => {
            opInBufferOrder (valin._1)(ins) foreach { idx => {
              nodeIDOrderMap(0)(layerNodeIDN("layer0")(valin._1)) += idx->layerNodeIDN("input")(ins)
            }}
          }}
        }
      }}
    }

    for (i <- 1 to layerMap.size-2) {
      val layerNum = s"layer$i"
      val preLayerName = s"layer${i-1}"
      val preLayerNum = i-1
      val prepreLayerName = s"layer${i-2}"
      layerNodeIDN += layerNum->HashMap[String, Int]()
      nodeOrderMap += layerNum->HashMap[String, HashSet[String]]()


      current_layer = opTypeClass(layerMap(layerNum))
      var current_layer_order = ArrayBuffer[String]()

      current_layer foreach { case (opType, ops) => {
        ops foreach { op => {
            current_layer_order += op
        }}
      }}

      for (i <- inc until maxL) {
        freeIdx += i
      }
      if (dtmSignalCollect.nonEmpty) {
        dtmSignalCollect foreach {case(op, idx)=>{
          freeIdx -= idx
        }}
      }

      current_layer_order foreach { op => {
        nodeOrderMap(layerNum) += op->HashSet[String]()

        if (layerMap(preLayerName).contains(op)) {
          nodeOrderMap(layerNum)(op) += op
        }
        else {
          if (iNeigh.contains(op)) {
            iNeigh(op) foreach {ins => {
              if (layerMap(preLayerName).contains(ins))
                nodeOrderMap(layerNum)(op) += ins
            }}
          }
          if (oEBuffer.contains(op)) {
            oEBuffer(op) foreach {ins => {
              nodeOrderMap(layerNum)(op) += ins
            }}
          }
          if (iEBufferR.contains(op)) {
            nodeOrderMap(layerNum)(op) ++= iEBufferR(op).clone()
          }
        }
        val opIns = nodeOrderMap(layerNum)(op)

        if (layerNodeIDN(preLayerName).contains(op)) {
          val last_op_pos = layerNodeIDN(preLayerName)(op)
          layerNodeIDN(layerNum) += op->last_op_pos
          freeIdx -= last_op_pos
        } 
        else if (regUpOpReg.contains(op)) {
          layerNodeIDN(layerNum) += op->newregorder(regUpOpReg(op))
          freeIdx -= newregorder(regUpOpReg(op))
        }
        else if (!op.contains("stop") && !op.contains("memw") && SimDTM_output.contains(oEBufferR(op))) {
          val simdtm_out = actual_dtmin + SimDTM_output.indexOf(oEBufferR(op))
          layerNodeIDN(layerNum) += op->simdtm_out
          dtmSignalCollect(oEBufferR(op)) = simdtm_out
          freeIdx -= simdtm_out
        }
        else {
          reduce_arr += op
        }
      }}

      val sortedfreeIdx = freeIdx.toSeq.sorted
      var freeIdx_idx = 0

      reduce_arr foreach { op=> {
        if (!dshiftCheck(op) && !op.startsWith("longbits")) {
          if (!layerNodeIDN(layerNum).contains(op)) {
            if (freeIdx_idx < sortedfreeIdx.size) {
              val cur_idx = sortedfreeIdx(freeIdx_idx)
              layerNodeIDN(layerNum) += op->cur_idx
              freeIdx_idx += 1
              freeIdx -= cur_idx
              reduceop += op
            } else {
              maxL += 1
              val cur_idx = maxL
              layerNodeIDN(layerNum) += op->cur_idx
              freeIdx_idx += 1
              freeIdx -= cur_idx
              reduceop += op
            }
          } 
        }
      }}

      reduce_arr --= reduceop

      reduce_arr foreach {op=> {
        if (dshiftCheck(op)) {
          if (!dshiftUsed.contains(op)) {
            var innerMap = HashMap[Int, HashSet[Int]]()
            val group = dshiftExtract(op)
            dshiftUsed ++= group
            val idx_list = findContiguousSet(freeIdx, group.size)
            if (idx_list.isDefined) {
              for (id <- 0 until group.size) {
                layerNodeIDN(layerNum) += group(id)->idx_list.get(id)
                freeIdx -= idx_list.get(id)
              }
              val key = idx_list.get.head
              val valueSet = HashSet(idx_list.get.tail: _*)
              innerMap(key) = valueSet
            } else {
              val increase_list = makeIncreasingBuffer(maxL, group.size)
              for (id <- 0 until group.size) {
                layerNodeIDN(layerNum) += group(id)->increase_list(id)
                freeIdx -= increase_list(id)
              }
              val key = increase_list.head
              val valueSet = HashSet(increase_list.tail: _*)
              innerMap(key) = valueSet
              maxL += group.size
            }
            dshiftOrder.getOrElseUpdate(i, HashMap.empty) ++= innerMap
          }
        } else {
          assert(false)
        }
      }}

      reduceop.clear()
      freeIdx.clear()
      reduce_arr.clear()
      current_layer_order.clear()

      if (i == 1) {
        memsave = layerNodeIDN("layer0").clone()
        layerNodeIDN("input") foreach {node=>{
          if (layerNodeIDN("layer0").contains(node._1)) {
            layerNodeIDN("layer0").remove(node._1)
          }
        }}

        nodeIDOrderMap += 0->HashMap[Int, HashMap[Int, Int]]()
        nodeOrderMap("layer0") foreach { valin => {
          if (layerNodeIDN("layer0").contains(valin._1)) {
            nodeIDOrderMap(0) += layerNodeIDN("layer0")(valin._1)->HashMap[Int, Int]()
            valin._2 foreach { ins => {
              opInBufferOrder (valin._1)(ins) foreach { idx => {
                nodeIDOrderMap(0)(layerNodeIDN("layer0")(valin._1)) += idx->layerNodeIDN("input")(ins)
              }}
            }}
          }
        }}
      }
      else {
        memsave2 = layerNodeIDN(preLayerName).clone()
        memsave foreach {node=>{
          if (layerNodeIDN(preLayerName).contains(node._1)) {
            if (layerNodeIDN(preLayerName)(node._1)==memsave(node._1))
              layerNodeIDN(preLayerName).remove(node._1)
          }
        }}
        
        nodeIDOrderMapUpdate(nodeOrderMap, preLayerNum, preLayerName, prepreLayerName)
        memsave.clear()
        memsave = memsave2.clone()
      }
    }
    if (layerMap.size > 2) {
      val lastLayer = s"layer${layerMap.size-2}"
      val lastLayerNum = layerMap.size-2
      val secLastLayer = s"layer${layerMap.size-3}"
      val secLastLayerNum = layerMap.size-3

      rmvIOPs(memsave, lastLayer, secLastLayer)
      nodeIDOrderMapUpdate(nodeOrderMap, lastLayerNum, lastLayer, secLastLayer)
    }

    memsave.clear()
    memsave2.clear()
  }

  def rmvIOPs(prelayer: HashMap[String, Int], currLayer: String, preLayerName: String): Unit = {
    prelayer foreach {node=>{
      if (layerNodeIDN(currLayer).contains(node._1)){
        if (layerNodeIDN(currLayer)(node._1) == prelayer(node._1)) {
          layerNodeIDN(currLayer).remove(node._1)
        }
      }}
    }
  }

  def getRealMaxL(): Unit = {
    maxL = 0
    layerNodeIDN foreach {case(layerN, ops)=>{
      ops foreach {op=>{
        if (op._2 >= maxL) {maxL = op._2+1}
      }}
    }}
  }

  def tensorMetadataUpdate(): Unit = {
    dshiftOrder foreach {case(layer, group)=>{
      group foreach{case(lead_shift, follow_shift)=>{
        follow_shift foreach {rvm_s=>{
          nodeIDOrderMap(layer).remove(rvm_s)
          // isnMap(layer).remove(rvm_s)
        }}
      }}
    }}

    longbitsOrder foreach {case(layer, group)=>{
      group foreach{case(lead_shift, follow_shift)=>{
        follow_shift foreach {rvm_s=>{
          nodeIDOrderMap(layer).remove(rvm_s)
        }}
      }}
    }}

    isnMap foreach {case(layer, sn)=>{
      sn foreach {case(s, n)=>{
        if (n == opIndex("chain")) {
          nodeIDOrderMap(layer)(s).filterInPlace { case (k, _) => k <= 2 }
        }
      }}
    }}

    val layersToRemove = nodeIDOrderMap.collect {
      case (layer, ops) if ops.isEmpty =>
        assert(isnMap(layer).isEmpty, s"Layer $layer has non-empty isnMap but empty nodeIDOrderMap")
        assert(layerNodeIDN(s"layer$layer").isEmpty, s"Layer $layer has non-empty layerNodeIDN but empty nodeIDOrderMap")
        layer
    }.toList.sorted

    LAYER_NUM -= layersToRemove.size

    layersToRemove.foreach { layer =>
      nodeIDOrderMap.remove(layer)
      isnMap.remove(layer)
      layerNodeIDN.remove(s"layer$layer")
    }

    var shift = 0
    var removedIdx = 0

    val sortedRemainingLayers = nodeIDOrderMap.keys.toList.sorted
    sortedRemainingLayers.foreach { layer =>
      while (removedIdx < layersToRemove.length && layersToRemove(removedIdx) < layer) {
        shift += 1
        removedIdx += 1
      }
      if (shift > 0) {
        val newLayer = layer - shift

        nodeIDOrderMap(newLayer) = nodeIDOrderMap.remove(layer).get
        isnMap(newLayer) = isnMap.remove(layer).get
        layerNodeIDN(s"layer$newLayer") = layerNodeIDN.remove(s"layer$layer").get
      }
    }
  }

  def dshiftLayerCheck(): ArrayBuffer[Boolean] = {
    val res = ArrayBuffer[Boolean]()
    dshiftGroup foreach {group=>{
      val matchingKeys = layerMap.filter { case (_, set) =>
        group.forall(set.contains)
      }.keys
      res += matchingKeys.nonEmpty
      if (!matchingKeys.nonEmpty) {
      }
    }}
    res
  }

  def debugMapGen(): Unit = {
    layerNodeIDN foreach { case (layer, ops) => {
      if (layer != "input") {
        val layerNum = layer.replace("layer", "").toInt
        ops foreach {op=>{
          val outputval = if (oEBufferR.contains(op._1)) oEBufferR(op._1) else ""
          if (!regOrder.contains(outputval) && !regNextOrder.contains(outputval) && outputval != "" && !op._1.contains("memr")) {
            val new_outputval = outputval.replace(".", "$")
            if (!debugMap.contains(layerNum)) {
              debugMap(layerNum) = scala.collection.mutable.HashMap[Int, String]()
            }
            debugMap(layerNum)(op._2) = new_outputval
          }
        }}
      }
    }}
  }

  def layerInfoPrint(): Unit = {
    val sortedFilteredSeq = layerNodeIDN
      .filter { case (k, _) => k.startsWith("layer") }
      .toSeq
      .sortBy { case (k, _) => k.stripPrefix("layer").toInt }
      .map { case (k, inner) =>
        k -> HashSet(inner.keys.toSeq: _*)
      }

    sortedFilteredSeq.foreach { case (layer, ops) =>
      println(s"$layer: ${ops.size} ops")
      println(ops.mkString(", "))
      println()
    }
  }

  def DFGraphGen(): Unit = {
    val reg_extra_inputs = reg_update()
    var outEdgeBuffer_cp = new BufferHashMap()
    outEdgeBuffer foreach (kv => outEdgeBuffer_cp += kv._1->kv._2.clone())
    opGraphConstructor(inEdgeBuffer , outEdgeBuffer)
    var outNeigh_cp = new BufferHashMap()
    outNeigh foreach (kv => outNeigh_cp += kv._1->kv._2.clone())
    var inNeigh_cp = new BufferHashMap()
    inNeigh foreach (kv => inNeigh_cp += kv._1->kv._2.clone())
    layerExtract(outNeigh_cp, inNeigh_cp, inEdgeBuffer , outEdgeBuffer )

    println("done layer extract")
    rvmExtraMemInput(reg_extra_inputs)
    iEBufferReverse(inEdgeBuffer, iEBufferR)


    addAllIOP(outEdgeBuffer_cp)
    println("done iop adder output", TOTAL_IOP)
    inNeigh_cp.clear()
    outNeigh_cp.clear()

    getMaxL()
    dshiftLayerCheck()

    opIndexAssign()

    orderAssign(inEdgeBuffer , outEdgeBuffer , inNeigh, nodeToLayer, opInBufferOrder)
    println("done orderassign")
    inNeigh.clear()
    outNeigh.clear()
    opInBufferOrder.clear()
    layerMap.clear()
    nodeToLayer.clear()

    paramsExtract()
    tensorMetadataUpdate()
    getRealMaxL()

    for (i <- circuitInput.size until maxL) {
      circuitInput += BigInt(0)
    }

    debugMapGen()
    // layerInfoPrint()
  }
}


object TeAALDFGraphExtractor {
  type IOpArray = ArrayBuffer[HashSet[String]]
  type teAALHashMap = HashMap[String,HashMap[String, HashSet[String]]]
  type teAALIDHashMap = HashMap[Int,HashMap[Int, HashMap[Int, Int]]]
  type convHashMap = HashMap[String, HashMap[String, Int]]
  type paramTensor = HashMap[Int, HashMap[Int, HashMap[Int, String]]]
  type ISNMap = HashMap[Int,HashMap[Int, Int]]
}