package essent

import essent.ir._
import firrtl._
import firrtl.ir._
import firrtl.PrimOps._
import collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue, LinkedHashMap}


class TeAALExtractor(memory_array : LinkedHashMap[String, Int]) {

  import TeAALExtractor.{BufferHashMap, opInOrder}

  val ju = new TeAALUtil()

  var op_counter = 0
  var opInBufferOrder: opInOrder = HashMap[String, HashMap[String, HashSet[Int]]]()
  var inEdgeBuffer: BufferHashMap = HashMap[String, HashSet[String]]()
  var outEdgeBuffer: BufferHashMap = HashMap[String, HashSet[String]]()
  var oEBufferR = new HashMap[String, String]()
  var bitWidthMetadata = HashMap[String, Int]()

  // dataflow graph opt passes
  var regOrder = HashSet[String]()
  var regNextOrder = HashSet[String]()

  // break down
  var largeOpBreakDown = HashMap[String, HashMap[Int, String]]()
  var inOutMap = HashMap[String, String]()
  var dshiftGroup = HashSet[ArrayBuffer[String]]()
  var ORank: Int = 0

  // ================================ PART 1: Large Var Split ================================
  // Large UInt<> and SInt<> spliting function
  def splitTo64BitChunks(value: BigInt, bitWidth: Int, isSigned: Boolean): HashMap[Int, (Int, BigInt)] = {
    // Step 1: Mask value to declared bit width
    val maskedValue = {
      val mask = (BigInt(1) << bitWidth) - 1
      if (isSigned && value < 0) {
        // Convert to two's complement for signed negatives
        (value + (BigInt(1) << bitWidth)) & mask
      } else {
        value & mask
      }
    }

    // Step 2: Determine number of 64-bit chunks
    val numChunks = (bitWidth + 63) / 64

    // Step 3: Split into 64-bit chunks (LSB first)
    (0 until numChunks).map { i =>
      val chunk = (maskedValue >> (i * 64)) & ((BigInt(1) << 64) - 1)
      val chunkWidth = if (i < numChunks - 1) 64 else bitWidth % 64 match {
        case 0 => 64
        case rem => rem
      }
      i -> (chunkWidth, chunk)
    }.to(HashMap)
  }

  // Large variables spliting function
  def smallOpGeneration(opName: String, bw: Int): HashMap[Int, String] = {
    val OpNum = (bw + 63) / 64
    var opSet = HashMap[Int, String]()
    if (ju.isInte(opName)) { // be careful, maybe repeated
      opName match {
        case ju.Upattern(hexStr) => {
          splitTo64BitChunks(BigInt(hexStr, 16), bitWidthMetadata (opName), false) foreach { case (i, chunk) =>
            assert(chunk._2>=0)
            var opN = s"UInt<${chunk._1}>(\"h${chunk._2.toString(16)}\")"
            bitWidthMetadata  += opN->chunk._1
            opSet += i->opN
          }
        }
        case ju.Spattern(hexStr) => {
          splitTo64BitChunks(BigInt(hexStr, 16), bitWidthMetadata (opName), true) foreach { case (i, chunk) =>
            val neg = if (chunk._2 >= (BigInt(1) << (chunk._1 - 1))) "-" else ""
            val chunk2 = ju.signExtend(chunk._2, chunk._1)
            var opN = if (i < OpNum-1) s"UInt<${chunk._1}>(\"h${chunk._2.toString(16)}\")" else s"SInt<${chunk._1}>(\"h${neg}${chunk2.toString(16)}\")"
            bitWidthMetadata  += opN->chunk._1
            opSet += i->opN
          }
        }
        case _ => {
          throw new IllegalArgumentException("Invalid format")
        }
      }
    } else if (ju.isDecimalInt(opName)) {
      println("decimal case should never happen: ", opName)
    } else { // Shall i consider registers separately?
      for (i <- 0 until OpNum-1) {
        val opN = s"${opName}_+=+_$i" 
        bitWidthMetadata  += opN->64
        opSet += i->opN
      }
      val newBW = if (bw%64==0) 64 else bw % 64
      bitWidthMetadata  += s"${opName}_+=+_${OpNum-1}"->newBW
      opSet += (OpNum-1)->s"${opName}_+=+_${OpNum-1}"
    }
    opSet
  }

  // ================================ PART 2: Helper functions to update graph structure easily ================================

  def updateOp(op: String, inputs: Seq[(String, Int)], output: String, bw: Int): Unit = {
    inputs.foreach { case (in, idx) =>
      inEdgeBuffer.getOrElseUpdate(in, HashSet.empty) += op
      opInBufferOrder.getOrElseUpdate(op, HashMap.empty).getOrElseUpdate(in, HashSet.empty) += idx
    }
    bitWidthMetadata(output) = bw
    bitWidthMetadata(op) = bw
    outEdgeBuffer += output -> HashSet(op)
    oEBufferR += op -> output
  }

  def genOneInOp(opN: String, lhs: String, bw: Int): String = {
    val name = s"${opN}_+_${op_counter}"
    op_counter += 1
    updateOp(name, Seq(lhs->0), s"${name}_output", bw)
    s"${name}_output"
  }

  def genOneInOpOut(opN: String, lhs: String, output: String, bw: Int): Unit = {
    val name = s"${opN}_+_${op_counter}"
    op_counter += 1
    updateOp(name, Seq(lhs->0), output, bw)
  }

  def genTwoInOp(opN: String, lhs: String, rhs: String, bw: Int): String = {
    val name = s"${opN}_+_${op_counter}"
    op_counter += 1
    updateOp(name, Seq(lhs->0, rhs->1), s"${name}_output", bw)
    s"${name}_output"
  }

  def genTwoInOpOut(opN: String, lhs: String, rhs: String, output: String, bw: Int): Unit = {
    val name = s"${opN}_+_${op_counter}"
    op_counter += 1
    updateOp(name, Seq(lhs->0, rhs->1), output, bw)
  }

  def genThreeOp(opN: String, cond: String, tval: String, fval: String, out: String, bw: Int): Unit = {
    val name = s"${opN}_+_${op_counter}"
    op_counter += 1
    updateOp(name, Seq(cond->0, tval->1, fval->2), out, bw)
  }

  def genThreeInOp(opN: String, cond: String, tval: String, fval: String, bw: Int): String = {
    val name = s"${opN}_+_${op_counter}"
    op_counter += 1
    updateOp(name, Seq(cond->0, tval->1, fval->2), s"${name}_output", bw)
    s"${name}_output"
  }

  def ensureBreakdown(name: String, op_bw: Int): HashMap[Int, String] = {
    if (op_bw > 64) {
      val num_block = (op_bw+63) / 64
      val blocks = largeOpBreakDown.getOrElseUpdate(name, smallOpGeneration(name, op_bw))
      for (i <- 0 until num_block) {
        bitWidthMetadata(blocks(i)) = if (i < num_block - 1) 64 else op_bw - (num_block - 1) * 64
      }
      return HashMap(blocks.filter { case (k, _) => k < num_block }.toSeq: _*)
    }
    else {
      return HashMap(0->name)
    }
  }

  def addChainOps(i_range: Int, op_counter_for_chain_op: Int, opType: String, opN: String, actualOut: String): Unit = {
    var operand1 = s"${opN}_+_${op_counter_for_chain_op}_output"
    for (i <- 1 until i_range-1) {
      var chain_op = s"${opType}_+_${op_counter}"
      genTwoInOp(opType, operand1, s"${opN}_+_${op_counter_for_chain_op+i}_output", 1)
      operand1 = s"${chain_op}_output"
    }
    genTwoInOpOut(opType, operand1, s"${opN}_+_${op_counter_for_chain_op+i_range-1}_output", actualOut, 1)
  }

   // extract bitwidth and signness info for variables
  def extractMetadata(tpe: Type, key: String, updateOp: Boolean, operation: Option[String] = None): Unit = {
      val bitWidth = TeAALExtractor.IntBitWidth(tpe, Some(key), operation)
      bitWidthMetadata(key) = bitWidth
      bitWidthMetadata(operation.getOrElse("defaultKey")) = bitWidth
  }
  
  // update buffers that contain the graph connectivity info
  def bufferUpdate(iEBuffer: BufferHashMap, oEBuffer: BufferHashMap, iEName: String, oEName: String, operation: String): Unit = {
    val paramIdx = 0
    opInBufferOrder += operation->HashMap[String, HashSet[Int]]()
    iEBuffer.getOrElseUpdate(iEName, HashSet.empty) += operation
    opInBufferOrder(operation).getOrElseUpdate(iEName, HashSet.empty) += paramIdx
    oEBuffer.getOrElseUpdate(oEName, HashSet.empty) += operation
    oEBufferR += operation -> oEName
  }

  // generate bwinfo given optype(usint) and bitwidth
  def genBWInfo(usint: String, bw: Int): String = {
    if (usint == "UInt") {
      ((bw << 1) + 0).toString
    } else if (usint == "SInt") {
      ((bw << 1) + 1).toString
    } else {
      throw new IllegalArgumentException(s"Unsupported unsigned type: $usint")
    }
  }

  // ================================ PART 3: smaller function sets for large operations ================================
  def wideAddSub(opN: String, usint: String, left_opSet: HashMap[Int, String], right_opSet: HashMap[Int, String], small_outs: HashMap[Int, String]): Unit = {
    assert(left_opSet.size == right_opSet.size)
    bitWidthMetadata.getOrElseUpdate(genBWInfo("UInt", 1), 64)
    bitWidthMetadata.getOrElseUpdate(genBWInfo("SInt", 1), 64)
    bitWidthMetadata.getOrElseUpdate(genBWInfo("UInt", 64), 64)
    bitWidthMetadata.getOrElseUpdate(genBWInfo("SInt", 64), 64)
    // Add
    if (opN == "add") {
      val bwinfo = genBWInfo(usint, bitWidthMetadata(left_opSet(0)))
      genThreeOp("add", bwinfo, left_opSet(0), right_opSet(0), small_outs(0), bitWidthMetadata(small_outs(0)))
      bitWidthMetadata.getOrElseUpdate(bwinfo, 64)
      var carry = genTwoInOp("lt", small_outs(0), left_opSet(0), 1)
      val final_idx = left_opSet.size-1
      if (final_idx > 0) {
        for (i <- 1 until final_idx+1) {
          val bwinfo = genBWInfo(usint, bitWidthMetadata(left_opSet(i)))
          val add_out = genThreeInOp("add", bwinfo, left_opSet(i), right_opSet(i), bitWidthMetadata(small_outs(i)))
          bitWidthMetadata.getOrElseUpdate(bwinfo, 64)
          val add_out_bwinfo = genBWInfo(usint, bitWidthMetadata(add_out))
          genThreeOp("add", add_out_bwinfo, add_out, carry, small_outs(i), bitWidthMetadata(small_outs(i)))
          bitWidthMetadata.getOrElseUpdate(add_out_bwinfo, 64)
          carry = genTwoInOp("lt", small_outs(i), left_opSet(i), 1)
        }
      }
      if (small_outs.size > left_opSet.size) {
        if (usint == "UInt") {
          val bwinfo = genBWInfo(usint, 64)
          genOneInOpOut("assign", carry, small_outs(final_idx+1), bitWidthMetadata(small_outs(final_idx+1)))
        } else if (usint == "SInt") {
          val bwinfo = genBWInfo(usint, 64)
          val left_signed = genTwoInOp("shr", left_opSet(final_idx), "63", bitWidthMetadata(small_outs(final_idx+1)))
          val right_signed = genTwoInOp("shr", right_opSet(final_idx), "63", bitWidthMetadata(small_outs(final_idx+1)))
          val tmp_out = genThreeInOp("sub", bwinfo, carry, left_signed, bitWidthMetadata(small_outs(final_idx+1)))
          genThreeOp("sub", bwinfo, tmp_out, right_signed, small_outs(final_idx+1), bitWidthMetadata(small_outs(final_idx+1)))
        } else {
          throw new IllegalArgumentException(s"Unsupported unsigned type: $usint")
        }
      }
    }
    // Sub
    else if (opN == "sub") {
      val bwinfo = genBWInfo(usint, bitWidthMetadata(left_opSet(0)))
      genThreeOp("sub", bwinfo, left_opSet(0), right_opSet(0), small_outs(0), bitWidthMetadata(small_outs(0)))
      bitWidthMetadata.getOrElseUpdate(bwinfo, 64)
      var carry = genTwoInOp("lt", left_opSet(0), small_outs(0), 1)
      val final_idx = left_opSet.size-1
      if (final_idx > 0) {
        for (i <- 1 until final_idx+1) {
          val mask = ((BigInt(1) << bitWidthMetadata(left_opSet(i))) - 1).toString
          val bwinfo = genBWInfo(usint, bitWidthMetadata(left_opSet(i)))
          val sub_out = genThreeInOp("sub", bwinfo, left_opSet(i), right_opSet(i), bitWidthMetadata(small_outs(i)))
          bitWidthMetadata.getOrElseUpdate(bwinfo, 64)
          val sub_out_bwinfo = genBWInfo(usint, bitWidthMetadata(sub_out))
          genThreeOp("sub", sub_out_bwinfo, sub_out, carry, small_outs(i), bitWidthMetadata(small_outs(i)))
          bitWidthMetadata.getOrElseUpdate(sub_out_bwinfo, 64)
          carry = genTwoInOp("lt", left_opSet(i), small_outs(i), 1)
        }
      }
      if (small_outs.size > left_opSet.size) {
        if (usint == "UInt") {
          val bwinfo = genBWInfo(usint, 64)
          genOneInOpOut("assign", carry, small_outs(final_idx+1), bitWidthMetadata(small_outs(final_idx+1)))
        } else if (usint == "SInt") {
          val left_signed = genTwoInOp("shr", left_opSet(final_idx), "63", bitWidthMetadata(small_outs(final_idx+1)))
          val right_signed = genTwoInOp("shr", right_opSet(final_idx), "63", bitWidthMetadata(small_outs(final_idx+1)))
          val bwinfo = genBWInfo(usint, 64)
          val tmp_out = genThreeInOp("sub", bwinfo, right_signed, carry, bitWidthMetadata(small_outs(final_idx+1)))
          genThreeOp("sub", bwinfo, tmp_out, left_signed, small_outs(final_idx+1), bitWidthMetadata(small_outs(final_idx+1)))
        } else {
          throw new IllegalArgumentException(s"Unsupported unsigned type: $usint")
        }
      }
    }
    else {
      throw new IllegalArgumentException(s"Unsupported operation: $opN")
    }
  }

  def wideMul(opN: String, left_opSet: HashMap[Int, String], right_opSet: HashMap[Int, String], small_outs: HashMap[Int, String], leftbw: Int, rightbw: Int): Unit = {
    assert(opN == "mul" || opN == "mulS")
    var carry = ""
    val temp_result = Array.fill(small_outs.size)("")
    var left_extended_lower, left_extended_upper, right_extended_lower, right_extended_upper = ""
    bitWidthMetadata.getOrElseUpdate(genBWInfo("UInt", 1), 64)
    bitWidthMetadata.getOrElseUpdate(genBWInfo("UInt", 64), 64)
    if (opN == "mulS") {
      val left_signed = genTwoInOp("shr", left_opSet(left_opSet.size-1), "63", 63)
      val right_signed = genTwoInOp("shr", right_opSet(right_opSet.size-1), "63", 63)
      val bwinfo = genBWInfo("UInt", 64)
      val left_extended = genThreeInOp("sub", bwinfo, "0", left_signed, 64)
      val right_extended = genThreeInOp("sub", bwinfo, "0", right_signed, 64)
      left_extended_lower = genTwoInOp("and", left_extended, "4294967295", 32)
      left_extended_upper = genTwoInOp("shr", left_extended, "32", 32)
      right_extended_lower = genTwoInOp("and", right_extended, "4294967295", 32)
      right_extended_upper = genTwoInOp("shr", right_extended, "32", 32)
    }
    val left_bound = if (opN == "mulS") small_outs.size else left_opSet.size
    for (i <- 0 until left_bound) {
      val right_bound = if (opN == "mulS") small_outs.size - i else right_opSet.size
      for (j <- 0 until right_bound) {
        assert(i + j < small_outs.size)
        var carry_lower, carry_upper = ""
        val a_lower = if (i >= left_opSet.size) {
          assert(opN == "mulS")
          left_extended_lower
        } else {
          genTwoInOp("and", left_opSet(i), "4294967295", 32)
        }
        val a_upper = if (i >= left_opSet.size) {
          assert(opN == "mulS")
          left_extended_upper
        } else {
          genTwoInOp("shr", left_opSet(i), "32", 32)
        }
        val b_lower = if (j >= right_opSet.size) {
          assert(opN == "mulS")
          right_extended_lower
        } else {
          genTwoInOp("and", right_opSet(j), "4294967295", 32)
        }
        val b_upper = if (j >= right_opSet.size) {
          assert(opN == "mulS")
          right_extended_upper
        } else {
          genTwoInOp("shr", right_opSet(j), "32", 32)
        }
        // mul -> 0, mulS -> 1
        val prod_ll = genThreeInOp("mul", "0", a_lower, b_lower, 64)
        val prod_lu = genThreeInOp("mul", "0", a_lower, b_upper, 64)
        val prod_ul = genThreeInOp("mul", "0", a_upper, b_lower, 64)
        val prod_uu = genThreeInOp("mul", "0", a_upper, b_upper, 64)
        val prod_ll_lower = genTwoInOp("and", prod_ll, "4294967295", 32)
        val prod_ll_upper = genTwoInOp("shr", prod_ll, "32", 32)
        val prod_lu_lower = genTwoInOp("and", prod_lu, "4294967295", 32)
        val prod_lu_upper = genTwoInOp("shr", prod_lu, "32", 32)
        val prod_ul_lower = genTwoInOp("and", prod_ul, "4294967295", 32)
        val prod_ul_upper = genTwoInOp("shr", prod_ul, "32", 32)
        
        var result_lower, result_upper, lower_sum, lower_sum_lower, lower_sum_upper, upper_sum = ""
        val bwinfo = genBWInfo("UInt", 64)
        if (j != 0) {
          carry_lower = genTwoInOp("and", carry, "4294967295", 32)
          carry_upper = genTwoInOp("shr", carry, "32", 32)
        }
        if (temp_result(i+j) != "") {
          result_lower = genTwoInOp("and", temp_result(i+j), "4294967295", 32)
          result_upper = genTwoInOp("shr", temp_result(i+j), "32", 32)
          if (j == 0) {
            lower_sum = genThreeInOp("add", bwinfo, result_lower, prod_ll_lower, 64)
          } else {
            val tmp_add = genThreeInOp("add", bwinfo, result_lower, carry_lower, 64)
            lower_sum = genThreeInOp("add", bwinfo, tmp_add, prod_ll_lower, 64)
          }
          lower_sum_lower = genTwoInOp("and", lower_sum, "4294967295", 32)
          lower_sum_upper = genTwoInOp("shr", lower_sum, "32", 32)
          if (j == 0) {
            val tmp0 = genThreeInOp("add", bwinfo, result_upper, prod_ll_upper, 64)
            val tmp1 = genThreeInOp("add", bwinfo, tmp0, lower_sum_upper, 64)
            val tmp2 = genThreeInOp("add", bwinfo, tmp1, prod_lu_lower, 64)
            upper_sum = genThreeInOp("add", bwinfo, tmp2, prod_ul_lower, 64)
          } else {
            val tmp0 = genThreeInOp("add", bwinfo, result_upper, carry_upper, 64)
            val tmp1 = genThreeInOp("add", bwinfo, tmp0, prod_ll_upper, 64)
            val tmp2 = genThreeInOp("add", bwinfo, tmp1, lower_sum_upper, 64)
            val tmp3 = genThreeInOp("add", bwinfo, tmp2, prod_lu_lower, 64)
            upper_sum = genThreeInOp("add", bwinfo, tmp3, prod_ul_lower, 64)
          }
        } else {
          if (j == 0) {
            val tmp0 = genThreeInOp("add", bwinfo, prod_ll_upper, prod_lu_lower, 64)
            upper_sum = genThreeInOp("add", bwinfo, tmp0, prod_ul_lower, 64)
          } else {
            lower_sum = genThreeInOp("add", bwinfo, carry_lower, prod_ll_lower, 64)
            lower_sum_lower = genTwoInOp("and", lower_sum, "4294967295", 32)
            lower_sum_upper = genTwoInOp("shr", lower_sum, "32", 32)
            val tmp0 = genThreeInOp("add", bwinfo, carry_upper, prod_ll_upper, 64)
            val tmp1 = genThreeInOp("add", bwinfo, tmp0, lower_sum_upper, 64)
            val tmp2 = genThreeInOp("add", bwinfo, tmp1, prod_lu_lower, 64)
            upper_sum = genThreeInOp("add", bwinfo, tmp2, prod_ul_lower, 64)
          }
        }
        val upper_sum_upper = genTwoInOp("shr", upper_sum, "32", 32)
        val upper_sum_lower_up = genTwoInOp("shl", upper_sum, "32", 64)
        if (i == 0 && j == 0) {
          genTwoInOpOut("or", upper_sum_lower_up, prod_ll_lower, small_outs(0), bitWidthMetadata(small_outs(0)))
        } else {
          if (j == 0 || (i == left_opSet.size-1 && opN == "mul")) {
            genTwoInOpOut("or", upper_sum_lower_up, lower_sum_lower, small_outs(i+j), bitWidthMetadata(small_outs(i+j)))
          } else {
            temp_result(i+j) = genTwoInOp("or", upper_sum_lower_up, lower_sum_lower, bitWidthMetadata(small_outs(i+j)))
          }
        }
        val tmp0 = genThreeInOp("add", bwinfo, upper_sum_upper, prod_lu_upper, 64)
        val tmp1 = genThreeInOp("add", bwinfo, tmp0, prod_ul_upper, 64)
        carry = genThreeInOp("add", bwinfo, tmp1, prod_uu, 64)
      }
      if (opN == "mul") {
        assert(opN != "mulS")
        if (i == left_opSet.size-1) {
          if (i + right_opSet.size < small_outs.size) {
            genOneInOpOut("assign", carry, small_outs(i + right_opSet.size), bitWidthMetadata(small_outs(i + right_opSet.size)))
          }
        } else {
          if (temp_result(i + right_opSet.size) != "") {
            val bwinfo = genBWInfo("UInt", 64)
            temp_result(i + right_opSet.size) = genThreeInOp("add", bwinfo, temp_result(i + right_opSet.size), carry, bitWidthMetadata(small_outs(i + right_opSet.size)))
          } else {
            temp_result(i + right_opSet.size) = genOneInOp("assign", carry, bitWidthMetadata(small_outs(i + right_opSet.size)))
          }
        }
      }
    }
  }

  def wideLessThan(opN: String, left_opSet: HashMap[Int, String], right_opSet: HashMap[Int, String], opOut: String): Unit = {
    assert(left_opSet.size == right_opSet.size)
    val lt_array = Array.fill(left_opSet.size)("")
    val eq_array = Array.fill(left_opSet.size)("")
    // compute less than and equal
    for (i <- 0 until left_opSet.size) {
      lt_array(i) = genTwoInOp("lt", left_opSet(i), right_opSet(i), 1)
      eq_array(i) = genTwoInOp("eq", left_opSet(i), right_opSet(i), 1)
    }
    // compute unsigned less than result
    var lt_result = "0"
    for (i <- 0 until left_opSet.size) {
      // if unsigned, directly get result here
      if (opN == "lt" && i == left_opSet.size - 1) {
        genThreeOp("mux", eq_array(i), lt_result, lt_array(i), opOut, 1)
        return
      } else {
        lt_result = genThreeInOp("mux", eq_array(i), lt_result, lt_array(i), 1)
      }
    }
    assert(opN == "ltS")
    // check if lhs rhs negative
    val left_neg = genTwoInOp("ltS", left_opSet(left_opSet.size - 1), "0", 1)
    val right_neg = genTwoInOp("ltS", right_opSet(right_opSet.size - 1), "0", 1)
    val parity = genTwoInOp("xor", left_neg, right_neg, 1)
    // 1 if left_neg is true
    val parity_mux = genThreeInOp("mux", left_neg, "1", "0", 1)
    // parity is true -> parity mux, else -> lt_result
    genThreeOp("mux", parity, parity_mux, lt_result, opOut, 1)
  }

  def wideLessOrEqual(opN: String, left_opSet: HashMap[Int, String], right_opSet: HashMap[Int, String], opOut: String): Unit = {
    assert(left_opSet.size == right_opSet.size)
    val lt_array = Array.fill(left_opSet.size)("")
    val eq_array = Array.fill(left_opSet.size)("")
    // compute less than and equal
    for (i <- 0 until left_opSet.size) {
      lt_array(i) = genTwoInOp("lt", left_opSet(i), right_opSet(i), 1)
      eq_array(i) = genTwoInOp("eq", left_opSet(i), right_opSet(i), 1)
    }
    // compute unsigned less than eq result
    var lte_result = "1"
    for (i <- 0 until left_opSet.size) {
      // if unsigned, directly get result here
      if (opN == "leq" && i == left_opSet.size - 1) {
        genThreeOp("mux", eq_array(i), lte_result, lt_array(i), opOut, 1)
        return
      } else {
        lte_result = genThreeInOp("mux", eq_array(i), lte_result, lt_array(i), 1)
      }
    }
    assert(opN == "leqS")
    // check if lhs rhs negative
    val left_neg = genTwoInOp("ltS", left_opSet(left_opSet.size - 1), "0", 1)
    val right_neg = genTwoInOp("ltS", right_opSet(right_opSet.size - 1), "0", 1)
    val parity = genTwoInOp("xor", left_neg, right_neg, 1)
    val parity_mux = genThreeInOp("mux", left_neg, "1", "0", 1)
    genThreeOp("mux", parity, parity_mux, lte_result, opOut, 1)
  }

  def wideEqNeq(opN: String, left_opSet: HashMap[Int, String], right_opSet: HashMap[Int, String], opOut: String): Unit = {
    assert(left_opSet.size == right_opSet.size)

    val op_counter_for_chain_op = op_counter 
    for (i <- 0 until left_opSet.size) {
      genTwoInOp(opN, left_opSet(i), right_opSet(i), 1)
    }
    if (opN == "eq") {
      addChainOps(left_opSet.size, op_counter_for_chain_op, "and", opN, opOut)
    } else if (opN == "neq") {
      addChainOps(left_opSet.size, op_counter_for_chain_op, "or", opN, opOut)
    }
  }

  def wideShl(opN: String, left_opSet: HashMap[Int, String], shift_const: Int, small_outs: HashMap[Int, String]): Unit = {
    val shift_block = shift_const / 64
    val shift_len = shift_const % 64
    val total_block = small_outs.size
    val shr_q = Queue[String]()

    for (i <- 0 until shift_block) {
      genOneInOpOut("assign", "0", small_outs(i), bitWidthMetadata(small_outs(i)))
    }
    genTwoInOpOut(opN, left_opSet(0), shift_len.toString, small_outs(shift_block), bitWidthMetadata(small_outs(shift_block)))
    for (i <- 0 until left_opSet.size-1) {
      assert(64-shift_len != 0, s"Shift length ${64-shift_len} should not be 0")
      val shr_output = genTwoInOp("shr", left_opSet(i), (64-shift_len).toString, shift_len)
      shr_q.enqueue(shr_output)
    }
    for (i <- 1 until left_opSet.size) {
      if (shift_len == 64) {
        genOneInOpOut("assign", shr_q.dequeue(), small_outs(i+shift_block), bitWidthMetadata(small_outs(i+shift_block)))
      } else {
        assert(shift_len != 64, s"Shift length $shift_len should not be 64")
        genThreeOp("cat", left_opSet(i), shr_q.dequeue(), shift_len.toString, small_outs(i+shift_block), bitWidthMetadata(small_outs(i+shift_block)))
      }
    }
    if (total_block > (left_opSet.size + shift_block)) {
      assert(64-shift_len != 0, s"Shift length ${64-shift_len} should not be 0")
      genTwoInOpOut("shr", left_opSet(left_opSet.size-1), (64-shift_len).toString, small_outs(small_outs.size-1), bitWidthMetadata(left_opSet(left_opSet.size-1))-(64-shift_len))
    } 
  }

  def wideShr(opN: String, left_opSet: HashMap[Int, String], shift_const: Int, small_outs: HashMap[Int, String]): Unit = {
    val shift_block = shift_const / 64
    val shift_len = shift_const % 64
    val total_block = small_outs.size
    
    for (i <- shift_block until (left_opSet.size - 1)) {
      val shr_output = if (shift_len == 0) {
        left_opSet(i)
      } else {
        assert(shift_len != 0, s"Shift length $shift_len should not be 0")
        genTwoInOp("shr", left_opSet(i), shift_len.toString, 64)
      }
      // Upper chunk concate to the current chunk      
      if (shift_len == 0) {
        genOneInOpOut("assign", shr_output, small_outs(i-shift_block), 64)
      } else {
        assert(64-shift_len != 64, s"Shift length ${64-shift_len} should not be 64")
        genThreeOp("cat", left_opSet(i+1), shr_output, (64-shift_len).toString, small_outs(i-shift_block), 64)
      }
    }
    // Last chunk
    if (total_block == (left_opSet.size - shift_block)) {
      genTwoInOpOut(opN, left_opSet(left_opSet.size-1), shift_len.toString, small_outs(total_block-1), bitWidthMetadata(left_opSet(left_opSet.size-1))-shift_len)
    }
  }

  def wideAndOrXor(opN: String, left_opSet: HashMap[Int, String], right_opSet: HashMap[Int, String], opOut_set: HashMap[Int, String]): Unit = {
    assert(left_opSet.size == right_opSet.size)
    for (i <- 0 until left_opSet.size) {
      genTwoInOpOut(opN, left_opSet(i), right_opSet(i), opOut_set(i), bitWidthMetadata(left_opSet(i)))
    }
  }

  def wideAssign(opN: String, left_opSet: HashMap[Int, String], opOut_set: HashMap[Int, String]): Unit = {
    assert(left_opSet.size == opOut_set.size)
    for (i <- 0 until left_opSet.size) {
      genOneInOpOut(opN, left_opSet(i), opOut_set(i), bitWidthMetadata(left_opSet(i)))
    }
  }

  def wideNot(opN: String, left_opSet: HashMap[Int, String], opOut_set: HashMap[Int, String]): Unit = {
    assert(left_opSet.size == opOut_set.size)
    for (i <- 0 until left_opSet.size) {
      val bw = bitWidthMetadata(left_opSet(i))
      val mask = ((BigInt(1) << bw) - 1).toString
      genTwoInOpOut(opN, left_opSet(i), mask, opOut_set(i), bitWidthMetadata(left_opSet(i)))
    }
  }

  def wideReductive(opN: String, left_opSet: HashMap[Int, String], opOut: String): Unit = {
    val op_counter_for_chain_op = op_counter 
    for (i <- 0 until left_opSet.size) {
      genOneInOp(opN, left_opSet(i), 1)
    }
    val update_op = opN.dropRight(1)
    addChainOps(left_opSet.size, op_counter_for_chain_op, update_op, opN, opOut)
  }

  def wideCat(opN: String, left_opSet: HashMap[Int, String], right_opSet: HashMap[Int, String], sec_bw: Int, small_outs: HashMap[Int, String]): Unit = {
    val shift_block = sec_bw / 64
    val shift_len = sec_bw % 64
    val total_block = small_outs.size
    val shr_q = Queue[String]()

    for (i <- 0 until right_opSet.size-1) {
      genOneInOpOut("assign", right_opSet(i), small_outs(i), bitWidthMetadata(small_outs(i)))
    }
    assert(shift_len != 64, s"Shift length $shift_len should not be 64")
    genThreeOp("cat", left_opSet(0), right_opSet(right_opSet.size-1), shift_len.toString, small_outs(shift_block), bitWidthMetadata(small_outs(shift_block)))

    for (i <- 0 until left_opSet.size-1) {
      assert(64-shift_len != 0, s"Shift length ${64-shift_len} should not be 0")
      val shr_output = genTwoInOp("shr", left_opSet(i), (64-shift_len).toString, bitWidthMetadata(left_opSet(i))-(64-shift_len))
      shr_q.enqueue(shr_output)
    }

    for (i <- 1 until left_opSet.size) {
      assert(shift_len != 64, s"Shift length $shift_len should not be 64")
      genThreeOp("cat", left_opSet(i), shr_q.dequeue(), shift_len.toString, small_outs(i+shift_block), bitWidthMetadata(small_outs(i+shift_block)))
    }

    if (total_block > (left_opSet.size + shift_block)) {
      assert(64-shift_len != 0, s"Shift length ${64-shift_len} should not be 0")
      genTwoInOpOut("shr", left_opSet((left_opSet.size-1)), (64-shift_len).toString, small_outs(small_outs.size-1), bitWidthMetadata(small_outs(small_outs.size-1)))
    } 
  }

  def wideBits(opN: String, left_opSet: HashMap[Int, String], endIdx_const: Int, startIdx_const: Int, small_outs: HashMap[Int, String], bw_op: Int): Unit = {
    val startIdx_block = startIdx_const / 64
    val endIdx_block = endIdx_const / 64
    val cross_block = endIdx_block - startIdx_block + 1
    val result_block = ((endIdx_const - startIdx_const+64)/64)

    if (startIdx_block == endIdx_block) {
      val mask_len = endIdx_const - startIdx_const + 1
      // if mask length is 64, no need to apply mask
      if (mask_len == 64) {
        genOneInOpOut("assign", left_opSet(startIdx_block), small_outs(0), bitWidthMetadata(small_outs(0)))
      } else {
        assert(mask_len < 64, s"Mask length $mask_len should be less than 64")
        val mask = ((BigInt(1) << mask_len) - 1).toString
        genThreeOp(opN, left_opSet(startIdx_block), (startIdx_const-startIdx_block*64).toString, mask, small_outs(0), bitWidthMetadata(small_outs(0)))
      }
    } else if (endIdx_block == startIdx_block+1) {
      val bw = if (bw_op <= 64) bw_op else 64
      // If startIdx is aligned to 64-bit chunk, no need to shift
      val shr_output = if (startIdx_const % 64 == 0) {
        left_opSet(startIdx_block)
      } else {
        assert(startIdx_const-startIdx_block*64 != 0, s"Start index ${startIdx_const-startIdx_block*64} should not be 0")
        genTwoInOp("shr", left_opSet(startIdx_block), (startIdx_const-startIdx_block*64).toString, bw)
      }
      if (bw_op <= 64) {
        val shift_amount = (startIdx_block+1)*64-startIdx_const
        assert(shift_amount != 64, s"Shift amount $shift_amount should not be 64")
        genThreeOp("cat", left_opSet(endIdx_block), shr_output, ((startIdx_block+1)*64-startIdx_const).toString, small_outs(0), bw_op)
      } else {
        val shift_amount = (startIdx_block+1)*64-startIdx_const
        val shift_amount_2 = (startIdx_const-startIdx_block*64)
        if (startIdx_const % 64 == 0) {
          genOneInOpOut("assign", shr_output, small_outs(0), 64)
        } else {
          assert(shift_amount != 64, s"Shift amount $shift_amount should not be 64")
          genThreeOp("cat", left_opSet(endIdx_block), shr_output, shift_amount.toString, small_outs(0), 64)
        }
        if (shift_amount_2 == 0) {
          genOneInOpOut("assign", left_opSet(endIdx_block), small_outs(1), bw_op-64)
        } else {
          assert(shift_amount_2 != 0, s"Shift amount $shift_amount_2 should not be 0")
          genTwoInOpOut("shr", left_opSet(endIdx_block), shift_amount_2.toString, small_outs(1), bw_op-64)
        }
      }
    } else {
      val shr_q = Queue[String]()
      for (b<-startIdx_block until endIdx_block) {
        val shr_output = if (startIdx_const % 64 == 0) {
          left_opSet(b)
        } else {
          assert(startIdx_const-startIdx_block*64 != 0, s"Start index ${startIdx_const-startIdx_block*64} should not be 0")
          genTwoInOp("shr", left_opSet(b), (startIdx_const-startIdx_block*64).toString, 64)
        }
        shr_q.enqueue(shr_output)
        if (b > startIdx_block) {
          val shift_amount = 64 - (startIdx_const-startIdx_block*64)
          if (startIdx_const % 64 == 0) {
            genOneInOpOut("assign", shr_q.dequeue(), small_outs(b-startIdx_block-1), 64)
          } else {
            assert(shift_amount != 64, s"Shift amount $shift_amount should not be 64")
            genThreeOp("cat", left_opSet(b), shr_q.dequeue(), shift_amount.toString, small_outs(b-startIdx_block-1), 64)
          }
        }
      }
      if (result_block < cross_block) {
        val shift_amount = (startIdx_block+1)*64-startIdx_const
        assert(shift_amount != 64, s"Shift amount $shift_amount should not be 64")
        genThreeOp("cat", left_opSet(endIdx_block), shr_q.dequeue(), shift_amount.toString, small_outs(endIdx_block-startIdx_block-1), 64)
      } else if (cross_block == result_block) {
        val shift_amount = 64 - (startIdx_const-startIdx_block*64)
        val shift_amount_2 = (startIdx_const-startIdx_block*64)
        if (startIdx_const % 64 == 0) {
          genOneInOpOut("assign", shr_q.dequeue(), small_outs(endIdx_block-startIdx_block-1), 64)
        } else {
          assert(shift_amount != 64, s"Shift amount $shift_amount should not be 64")
          genThreeOp("cat", left_opSet(endIdx_block), shr_q.dequeue(), shift_amount.toString, small_outs(endIdx_block-startIdx_block-1), 64)
        }
        assert(shift_amount_2 != 0, s"Shift amount $shift_amount_2 should not be 0")
        genTwoInOpOut("shr", left_opSet(endIdx_block), shift_amount_2.toString, small_outs(endIdx_block-startIdx_block), 64)
      }
    }
  }

  def wideMux(opN: String, condName: String, tvalChunks: HashMap[Int, String], fvalChunks: HashMap[Int, String], outChunks: HashMap[Int, String]): Unit = {
    assert(tvalChunks.size == fvalChunks.size)

    for (i <- 0 until outChunks.size) {
      val op = s"${opN}_+_${op_counter}"
      op_counter += 1
      if (opN == "assign")
        updateOp(op, Seq(tvalChunks(i) -> 0), outChunks(i), bitWidthMetadata(outChunks(i)))
      else
        updateOp(op, Seq(condName -> 0, tvalChunks(i) -> 1, fvalChunks(i) -> 2), outChunks(i), bitWidthMetadata(outChunks(i)))
    }
  }

  def wideDShift(opN: String, left_opSet: HashMap[Int, String], shift_const: String, small_outs: HashMap[Int, String], opOut: String): Unit = {
    val input_num = (left_opSet.size).toString
    bitWidthMetadata  += input_num->64

    if (left_opSet.size+3 > ORank) {
      ORank = left_opSet.size+4
    }

    val total_block = small_outs.size
    var dshiftOps = ArrayBuffer[String]()

    for (i<-0 until total_block) {
      val new_op = s"${opN}_+_${op_counter}"
      dshiftOps += new_op
      op_counter  = op_counter + 1
      opInBufferOrder  += new_op->HashMap[String, HashSet[Int]]()

      inEdgeBuffer.getOrElseUpdate(input_num, HashSet.empty) += new_op
      opInBufferOrder (new_op).getOrElseUpdate(input_num, HashSet.empty) += 0
      // println(s"Adding input edge for $new_op with input_num $input_num")

      inEdgeBuffer.getOrElseUpdate(shift_const, HashSet.empty) += new_op
      opInBufferOrder (new_op).getOrElseUpdate(shift_const, HashSet.empty) += 1

      val shift_const_bw = bitWidthMetadata(shift_const)
      val shift_mask = ((BigInt(1) << shift_const_bw) - 1).toString
      bitWidthMetadata(shift_mask) = 64
      inEdgeBuffer.getOrElseUpdate(shift_mask, HashSet.empty) += new_op
      opInBufferOrder (new_op).getOrElseUpdate(shift_mask, HashSet.empty) += 2

      val output_bw = bitWidthMetadata(opOut).toString
      inEdgeBuffer.getOrElseUpdate(output_bw, HashSet.empty) += new_op
      opInBufferOrder (new_op).getOrElseUpdate(output_bw, HashSet.empty) += 3

      for (o<-0 until left_opSet.size) {
        inEdgeBuffer.getOrElseUpdate(left_opSet(o), HashSet.empty) += new_op
        opInBufferOrder (new_op).getOrElseUpdate(left_opSet(o), HashSet.empty) += 4+o
      }
      outEdgeBuffer.getOrElseUpdate(small_outs(i), HashSet.empty) += new_op
      oEBufferR  += new_op -> small_outs(i)
      bitWidthMetadata  += new_op->64
      bitWidthMetadata  += small_outs(i)->64
    }
    dshiftGroup += dshiftOps
    assert((bitWidthMetadata(opOut)+63)/64 == total_block)
  }

  // ================================ PART 4: top function for each operation ================================
  // operation extraction function for add and subtract
  def addsubExtract(p: DoPrim, outName: String): Unit = {
    val opN = p.op match {
      case Add => "add"
      case Sub => "sub"
    }
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    val rhs = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.args(1).tpe, rhs, false)
    extractMetadata(p.tpe, outName, false)

    assert((bitWidthMetadata(lhs)+63)/64 == (bitWidthMetadata(rhs)+63)/64)
    assert(bitWidthMetadata(lhs) == bitWidthMetadata(rhs), s"Expected lhs and rhs to have same bit width, got lhs: ${bitWidthMetadata(lhs)}, rhs: ${bitWidthMetadata(rhs)}")
    assert(bitWidthMetadata(outName) == bitWidthMetadata(lhs)+1, s"Expected outName bit width to be lhs + 1, got outName: ${bitWidthMetadata(outName)}, lhs: ${bitWidthMetadata(lhs)}")
    val usint = (p.args(0).tpe, p.args(1).tpe) match {
      case (UIntType(_), UIntType(_)) => "UInt" // both are UInt, ok
      case (SIntType(_), SIntType(_)) => "SInt" // both are SInt, ok
      case _ => 
        throw new AssertionError(s"Expected both inputs to be SInt or UInt of any width, got lhs: ${p.args(0).tpe}, rhs: ${p.args(1).tpe}")
    }
    (p.tpe, p.args(0).tpe) match {
      case (UIntType(_), UIntType(_)) => // output is UInt, ok
      case (SIntType(_), SIntType(_)) => // output is SInt, ok
      case _ => 
        throw new AssertionError(s"Expected output type to be SInt or UInt of any width, got output: ${p.tpe}, input: ${p.args(0).tpe}")
    }

    val input_bw = bitWidthMetadata(lhs)
    val op_bw = bitWidthMetadata(outName)
    // if need to break the operations down into smaller pieces
    
    if (op_bw > 64) {
      val lhsChunks = ensureBreakdown(lhs, bitWidthMetadata(lhs))
      val rhsChunks = ensureBreakdown(rhs, bitWidthMetadata(rhs))
      val outputsChunks = ensureBreakdown(outName, op_bw)

      wideAddSub(opN, usint, lhsChunks, rhsChunks, outputsChunks)
    } 
    // no need to break down, record op directly
    else {
      if (lhs == rhs && opN == "sub") {
        updateOp(s"assign_+_${op_counter}", Seq("0" -> 0), outName, op_bw)
        op_counter += 1
      } else if ((ju.isInte(lhs) && ju.hexInt(lhs) == BigInt(0)) && (ju.isInte(rhs) && ju.hexInt(rhs) == BigInt(0)) ) {
        updateOp(s"assign_+_${op_counter}", Seq("0" -> 0), outName, op_bw)
        op_counter += 1
      } else if ((ju.isInte(lhs) && ju.hexInt(lhs) == BigInt(0)) && opN == "add") {
        updateOp(s"assign_+_${op_counter}", Seq(rhs -> 0), outName, op_bw)
        op_counter += 1
      } else if ((ju.isInte(rhs) && ju.hexInt(rhs) == BigInt(0))) {
        updateOp(s"assign_+_${op_counter}", Seq(lhs -> 0), outName, op_bw)
        op_counter += 1
      } else {
        val bwinfo = genBWInfo(usint, input_bw)
        updateOp(s"${opN}_+_${op_counter}", Seq(bwinfo->0, lhs -> 1, rhs -> 2), outName, op_bw)
        op_counter += 1
      }
    }
  }

  // operation extraction function for multiplication
  def mulExtract(p: DoPrim, outName: String): Unit = {
    val opN = p.tpe match {
      case u: UIntType => s"mul"
      case s: SIntType => s"mulS"
    }
    var lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    var rhs = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.args(1).tpe, rhs, false)
    extractMetadata(p.tpe, outName, false)

    if (bitWidthMetadata(lhs) < bitWidthMetadata(rhs)) {
      val tmp = lhs
      lhs = rhs
      rhs = tmp
    }

    val op_bw = bitWidthMetadata(outName)
    val lhs_bw = bitWidthMetadata(lhs)
    val rhs_bw = bitWidthMetadata(rhs)

    (p.args(0).tpe, p.args(1).tpe) match {
      case (UIntType(_), UIntType(_)) => // both are UInt, ok
      case (SIntType(_), SIntType(_)) => // both are SInt, ok
      case _ => 
        throw new AssertionError(s"Expected both inputs to be SInt or UInt of any width, got lhs: ${p.args(0).tpe}, rhs: ${p.args(1).tpe}")
    }
    (p.tpe, p.args(0).tpe) match {
      case (UIntType(_), UIntType(_)) => // output is UInt, ok
      case (SIntType(_), SIntType(_)) => // output is SInt, ok
      case _ => 
        throw new AssertionError(s"Expected output type to be SInt or UInt of any width, got output: ${p.tpe}, input: ${p.args(0).tpe}")
    }

    if (op_bw > 64) {
      val lhsChunks = ensureBreakdown(lhs, lhs_bw)
      val rhsChunks = ensureBreakdown(rhs, rhs_bw)
      val outputsChunks = ensureBreakdown(outName, op_bw)

      wideMul(opN, lhsChunks, rhsChunks, outputsChunks, lhs_bw, rhs_bw)
    } 
    else {
      // TODO: may need to handle ghost bits
      val constValue = opN match {
        case "mul" => "0"
        case "mulS" => "1"
      }
      updateOp(s"${opN}_+_${op_counter}", Seq(constValue->0, lhs -> 1, rhs -> 2), outName, op_bw)
      op_counter += 1
    }
  }

  // operation extraction function for division
  def divExtract(p: DoPrim, outName: String): Unit = {
    val opN = p.tpe match {
      case u: UIntType => s"div"
      case s: SIntType => s"divS"
    }
    var lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    var rhs = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.args(1).tpe, rhs, false)
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(outName)
    val lhs_bw = bitWidthMetadata(lhs)
    val rhs_bw = bitWidthMetadata(rhs)

    (p.args(0).tpe, p.args(1).tpe) match {
      case (UIntType(_), UIntType(_)) => // both are UInt, ok
      case (SIntType(_), SIntType(_)) => // both are SInt, ok
      case _ => 
        throw new AssertionError(s"Expected both inputs to be SInt or UInt of any width, got lhs: ${p.args(0).tpe}, rhs: ${p.args(1).tpe}")
    }
    (p.tpe, p.args(0).tpe) match {
      case (UIntType(_), UIntType(_)) => // output is UInt, ok
      case (SIntType(_), SIntType(_)) => // output is SInt, ok
      case _ => 
        throw new AssertionError(s"Expected output type to be SInt or UInt of any width, got output: ${p.tpe}, input: ${p.args(0).tpe}")
    }
    
    if (op_bw > 64) {
      throw new Exception("Division operation with bit width greater than 64 is not supported yet.")
    } else {
      // TODO: may need to handle ghost bits
      if (lhs == rhs) {
        updateOp(s"assign_+_${op_counter}", Seq("1" -> 0), outName, op_bw)
        op_counter += 1
      } else if (ju.isInte(lhs) && ju.hexInt(lhs) == BigInt(0)) {
        updateOp(s"assign_+_${op_counter}", Seq("0" -> 0), outName, op_bw)
        op_counter += 1
      } else {
        val constValue = opN match {
          case "div" => "2"
          case "divS" => "3"
        }
        updateOp(s"${opN}_+_${op_counter}", Seq(constValue->0, lhs -> 1, rhs -> 2), outName, op_bw)
        op_counter += 1
      }
    }
  }

  // operation extraction function for remainder
  def remExtract(p: DoPrim, outName: String): Unit = {
    val opN = p.tpe match {
      case u: UIntType => s"rem"
      case s: SIntType => throw new Exception("SInt remainder operation is not supported yet.")
    }
    var lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    var rhs = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.args(1).tpe, rhs, false)
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(outName)
    val lhs_bw = bitWidthMetadata(lhs)
    val rhs_bw = bitWidthMetadata(rhs)

    if (op_bw > 64) {
      throw new Exception("Remainder operation with bit width greater than 64 is not supported yet.")
    } else {
      // TODO: may need to handle ghost bits
      if (lhs == rhs) {
        updateOp(s"assign_+_${op_counter}", Seq("0" -> 0), outName, op_bw)
        op_counter += 1
      } else if (ju.isInte(lhs) && ju.hexInt(lhs) == BigInt(0)) {
        updateOp(s"assign_+_${op_counter}", Seq(rhs -> 0), outName, op_bw)
        op_counter += 1
      } else {
        updateOp(s"${opN}_+_${op_counter}", Seq("4"->0, lhs -> 1, rhs -> 2), outName, op_bw)
        op_counter += 1
      }
    }
  }

  // operation extraction function for greater than and greater than or equal to 
  def compareExtract(p: DoPrim, outName: String): Unit = {
    // Extract variable names via inOutMap
    val (lhs, rhs) = p.op match {
      case Lt | Leq =>
        val l = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
        val r = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
        (l, r) // use as-is
      case Gt | Geq =>
        val l = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
        val r = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
        (l, r) // reverse for gt/geq
      case _ =>
        throw new Exception(s"Unsupported operation: ${p.op}")
    }

    // Determine operation name based on op type and signedness
    val opN = (p.op, p.args(0).tpe) match {
      case (Lt | Gt,   _: UIntType)  => s"lt"
      case (Lt | Gt,   _: SIntType)  => s"ltS"
      case (Leq | Geq, _: UIntType)  => s"leq"
      case (Leq | Geq, _: SIntType)  => s"leqS"
      case _ => throw new Exception(s"Unsupported op/type combo: ${p.op}, ${p.tpe}")
    }

    // Metadata for input variables
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.args(1).tpe, rhs, false)

    // Metadata for output
    extractMetadata(p.tpe, outName, false)

    assert(bitWidthMetadata(outName) == 1)
    assert((bitWidthMetadata(lhs)+63)/64 == (bitWidthMetadata(rhs)+63)/64)
    
    val op_bw = bitWidthMetadata(lhs)
    assert(op_bw == bitWidthMetadata(rhs), s"Expected lhs and rhs to have same bit width, got lhs: ${bitWidthMetadata(lhs)}, rhs: ${bitWidthMetadata(rhs)}")
    // if need to break the operations down into smaller pieces

    if (lhs == rhs) {
      // Shortcut: if lhs and rhs are the same, lt is always false, leq is always true
      val constValue = opN match {
        case "lt" | "ltS" => "0"
        case "leq" | "leqS" => "1"
      }
      updateOp(s"assign_+_${op_counter}", Seq(constValue -> 0), outName, 1)
      op_counter += 1
    } else if (ju.isInte(lhs) && ju.hexInt(lhs) == BigInt(0) && opN == "leq") {
      updateOp(s"assign_+_${op_counter}", Seq("1" -> 0), outName, 1)
      op_counter += 1
    } else {
      if (op_bw > 64) {
        val lhsChunks = ensureBreakdown(lhs, op_bw)
        val rhsChunks = ensureBreakdown(rhs, op_bw)

        if (opN.contains("lt")) 
          wideLessThan(opN, lhsChunks, rhsChunks, outName)
        else if (opN.contains("leq")) 
          wideLessOrEqual(opN, lhsChunks, rhsChunks, outName)

      } 
      // no need to break down, record op directly
      else {
        // Update dependency graph
        updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, rhs -> 1), outName, 1)
        op_counter += 1

      }
    }
  }

  def eqneqExtract(p: DoPrim, outName: String): Unit = {
    val opN = p.op match {
      case Eq => "eq"
      case Neq => "neq"
      case Andr => "eq"
      case Orr => "neq"
    }
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(lhs)

    val rhs = p.op match {
      case Eq | Neq => inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
      case Andr => ((BigInt(1) << op_bw) - 1).toString
      case Orr => s"UInt<${op_bw}>(\"h0\")"
    }
    
    p.op match {
      case Eq | Neq => extractMetadata(p.args(1).tpe, rhs, false)
      case Andr => bitWidthMetadata(rhs) = op_bw
      case Orr => bitWidthMetadata(s"UInt<${op_bw}>(\"h0\")") = op_bw
    }

    assert((bitWidthMetadata(lhs)+63)/64 == (bitWidthMetadata(rhs)+63)/64)
    
    // if need to break the operations down into smaller pieces
    if (lhs == rhs) {
      // Shortcut: if lhs and rhs are the same, eq is always true, neq is always false
      val constValue = opN match {
        case "eq" => "1"
        case "neq" => "0"
      }
      updateOp(s"assign_+_${op_counter}", Seq(constValue -> 0), outName, 1)
      op_counter += 1
    } else {
      if (op_bw > 64) {
        val lhsChunks = ensureBreakdown(lhs, op_bw)
        val rhsChunks = ensureBreakdown(rhs, op_bw)
        wideEqNeq(opN, lhsChunks, rhsChunks, outName)
      } 
      // no need to break down, record op directly
      else {
        updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, rhs -> 1), outName, 1)
        op_counter += 1
      }
    }
  }

  def shlExtract(p: DoPrim, outName: String): Unit = {
    val opN = "shl"
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    val shift_const = p.consts.head.toInt
    extractMetadata(p.args(0).tpe, lhs, false)
    bitWidthMetadata(shift_const.toString) = 64
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(outName)
    assert((bitWidthMetadata(lhs)+shift_const+63)/64 == (op_bw+63)/64)
    
    if (op_bw > 64) {
      val lhsChunks = ensureBreakdown(lhs, bitWidthMetadata(lhs))
      val outputsChunks = ensureBreakdown(outName, op_bw)
      wideShl(opN, lhsChunks, shift_const, outputsChunks)
    } 
    // no need to break down, record op directly
    else {
      updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, shift_const.toString -> 1), outName, op_bw)
      op_counter += 1
    }
  }

  def shrExtract(p: DoPrim, outName: String): Unit = {
    val opN = p.tpe match {
      case u: UIntType => s"shr"
      case s: SIntType => s"shrS"
    }
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    val shift_const = p.consts.head.toInt
    extractMetadata(p.args(0).tpe, lhs, false)
    bitWidthMetadata(shift_const.toString) = 64
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(outName)
    assert((bitWidthMetadata(lhs)-shift_const+63)/64 == (op_bw+63)/64)
    
    if (bitWidthMetadata(lhs) > 64) {
      val lhsChunks = ensureBreakdown(lhs, bitWidthMetadata(lhs))
      val outputsChunks = ensureBreakdown(outName, op_bw)
      wideShr(opN, lhsChunks, shift_const, outputsChunks)
    } 
    // no need to break down, record op directly
    else {
      updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, shift_const.toString -> 1), outName, op_bw)
      op_counter += 1
    }
  }

  def andorxorExtract(p: DoPrim, outName: String): Unit = {
    val opN = p.op match {
      case And => "and"
      case Or => "or"
      case Xor => "xor"
    }
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    val rhs = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.args(1).tpe, rhs, false)
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(lhs)

    assert((bitWidthMetadata(rhs)+63)/64 == (op_bw+63)/64)
    assert((bitWidthMetadata(outName)+63)/64 == (op_bw+63)/64)
    
    if (op_bw > 64) {
      val lhsChunks = ensureBreakdown(lhs, op_bw)
      val rhsChunks = ensureBreakdown(rhs, op_bw)
      val outputsChunks = ensureBreakdown(outName, op_bw)
      wideAndOrXor(opN, lhsChunks, rhsChunks, outputsChunks)
    } 
    else {
      updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, rhs -> 1), outName, op_bw)
      op_counter += 1
    }
  }

  def notExtract(p: DoPrim, outName: String): Unit = {
    val opN = "xor"
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(outName)
    assert((bitWidthMetadata(lhs)+63)/64 == (op_bw+63)/64)
    
    if (op_bw > 64) {
      val lhsChunks = ensureBreakdown(lhs, op_bw)
      val outputsChunks = ensureBreakdown(outName, op_bw)
      wideNot(opN, lhsChunks, outputsChunks)
    } 
    else {
      val mask = ((BigInt(1) << op_bw) - 1).toString
      updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, mask->1), outName, op_bw)
      op_counter += 1
    }
  }

  def xorrExtract(p: DoPrim, outName: String): Unit = {
    val opN = "xorr"
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.tpe, outName, false)
    val op_bw = bitWidthMetadata(lhs)
    if (op_bw > 64) {
      val lhsChunks = ensureBreakdown(lhs, op_bw)
      wideReductive(opN, lhsChunks, outName)
    } 
    else {
      updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0), outName, 1)
      op_counter += 1
    }
  }

  // operation extraction function for concatenation
  def catExtract(p: DoPrim, outName: String): Unit = {
    val opN = "cat"
    val first_operand = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    val second_operand = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
    extractMetadata((p.args(0)).tpe, first_operand, false)
    extractMetadata((p.args(1)).tpe, second_operand, false)
    extractMetadata(p.tpe, outName, false)
    val sec_bw = bitWidthMetadata(second_operand)
    val output_bw = bitWidthMetadata(outName)

    if (ju.isInte(first_operand) && ju.hexInt(first_operand) == 0) {
      if ((sec_bw+63)/64 != (output_bw+63)/64) { // this case output_bw must > 64
        val input_breakdown = ensureBreakdown(second_operand, sec_bw).clone()
        val n = (output_bw+63)/64 - (sec_bw+63)/64
        val baseValue = "0"
        bitWidthMetadata("0") = 64
        val maxKey = input_breakdown.keys.maxOption.getOrElse(-1)
        val additions = (1 to n).map(i => (maxKey + i) -> baseValue).toMap
        val output_breakdown = input_breakdown ++ additions

        largeOpBreakDown.getOrElseUpdate(outName, output_breakdown)
      } else {
        inOutMap(outName) = second_operand
      }
    } else if (ju.isInte(second_operand) && ju.hexInt(second_operand) == 0) {
      if (output_bw <= 64) {
        val operation = s"shl_+_${op_counter}"
        op_counter += 1
        val bitWidth = sec_bw.toString
        bitWidthMetadata += bitWidth->64
        updateOp(operation, Seq(first_operand -> 0, bitWidth -> 1), outName, output_bw)
      } else {
        if (sec_bw%64 == 0) {
          val n = (sec_bw+63)/64
          val baseValue = "0"
          bitWidthMetadata("0") = 64
          val additions = (0 until n).map(i => i -> baseValue).toMap
          val maxKey = additions.keys.maxOption.getOrElse(-1) + 1
          val input_breakdown = ensureBreakdown(first_operand, sec_bw).clone()
          val updated = input_breakdown.map { case (k, v) => (k + maxKey) -> v }
          val output_breakdown = HashMap(additions.toSeq: _*) ++= updated

          largeOpBreakDown.getOrElseUpdate(outName, output_breakdown)
        } else {
          val lhsChunks = ensureBreakdown(first_operand, bitWidthMetadata(first_operand))
          val outputsChunks = ensureBreakdown(outName, output_bw)
          wideShl("shl", lhsChunks, sec_bw, outputsChunks)
        }
      }
    } else {
      if (output_bw > 64) {
        if (sec_bw%64 == 0) {
          val input_breakdown = ensureBreakdown(second_operand, sec_bw).clone()
          val higher_breakdown = ensureBreakdown(first_operand, bitWidthMetadata(first_operand))
          val n = (sec_bw+63)/64
          val updated = higher_breakdown.map { case (k, v) => (k + n) -> v }
          val output_breakdown = input_breakdown ++ updated

          if (!outName.contains("@next")) {
            largeOpBreakDown.getOrElseUpdate(outName, output_breakdown)
          } else {
            val reg_breakdown = ensureBreakdown(outName, output_bw)
            for (i <- 0 until reg_breakdown.size) {
              updateOp(s"assign_+_${op_counter}", Seq(output_breakdown(i) -> 0), reg_breakdown(i), bitWidthMetadata(reg_breakdown(i)))
              op_counter += 1
            }
          }
        } else {
          val lhsChunks = ensureBreakdown(first_operand, bitWidthMetadata(first_operand))
          val rhsChunks = ensureBreakdown(second_operand, sec_bw)
          val outputsChunks = ensureBreakdown(outName, output_bw)
          wideCat(opN,lhsChunks,  rhsChunks, sec_bw, outputsChunks)
        }
      } else {
        val operation = s"${opN}_+_${op_counter}"
        op_counter += 1
        updateOp(operation, Seq(first_operand -> 0, second_operand -> 1, sec_bw.toString->2), outName, output_bw)
      }
    }
  }

  def bitsExtract(p: DoPrim, outName: String): Unit = {
    var opN = "bits"
    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    val end_const = p.consts(0).toInt
    val start_const = p.consts(1).toInt
    extractMetadata(p.args(0).tpe, lhs, false)
    bitWidthMetadata(end_const.toString) = 64
    bitWidthMetadata(start_const.toString) = 64
    extractMetadata(p.tpe, outName, false)

    val op_bw = bitWidthMetadata(lhs)
    val output_bw = bitWidthMetadata(outName)

    val lhsChunks = ensureBreakdown(lhs, op_bw)
    val outputsChunks = ensureBreakdown(outName, output_bw)

    if (start_const == 0 && (end_const+1)%64 == 0) {
      opN = "assign"
      for (idx<-0 until outputsChunks.size) {
        updateOp(s"${opN}_+_${op_counter}", Seq(lhsChunks(idx) -> 0), outputsChunks(idx), bitWidthMetadata(outputsChunks(idx)))
        op_counter += 1
      }
    } else if (start_const == 0) {
      opN = "and"
      for (idx<-0 until outputsChunks.size) {
        val mask_len = bitWidthMetadata(outputsChunks(idx))
        val mask = (BigInt(1) << mask_len) - 1
        bitWidthMetadata(mask.toString) = mask_len
        updateOp(s"${opN}_+_${op_counter}", Seq(lhsChunks(idx) -> 0, mask.toString->1), outputsChunks(idx), bitWidthMetadata(outputsChunks(idx)))
        op_counter += 1
      }
    } else {
      if (op_bw > 64) {
        val lhsChunks = ensureBreakdown(lhs, op_bw)
        val outputsChunks = ensureBreakdown(outName, output_bw)
        wideBits(opN, lhsChunks, end_const, start_const, outputsChunks, output_bw)
      } 
      else {
        val mask = ((BigInt(1) << (end_const-start_const+1))-1).toString
        updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, start_const.toString->1,  mask -> 2), outName, output_bw)
        op_counter += 1
      }
    }
  }

  // operation extraction function for Mux
  def muxExtract(m: Mux, outName: String): Unit = {
    val cond = extractVar(m.cond)
    val tval = extractVar(m.tval)
    val fval = extractVar(m.fval)

    val condName = inOutMap.getOrElse(cond, cond)
    val tvalName = inOutMap.getOrElse(tval, tval)
    val fvalName = inOutMap.getOrElse(fval, fval)
    val opN = if (tvalName == fvalName) "assign" else "mux"
    // check the conditional variable bit width == 1
    // check the true var, false var bit width are the same
    assert(bitWidthMetadata(condName) == 1)
    assert((bitWidthMetadata(tvalName)+63)/64 == (bitWidthMetadata(fvalName)+63)/64)

    val op_bw = bitWidthMetadata(tvalName)
    bitWidthMetadata(outName) = op_bw
    // if need to break the operations down into smaller pieces
    if (op_bw > 64) {
      val tvalChunks = ensureBreakdown(tvalName, op_bw)
      val fvalChunks = ensureBreakdown(fvalName, op_bw)
      val outChunks = ensureBreakdown(outName, op_bw)
      wideMux(opN, condName, tvalChunks, fvalChunks, outChunks)
    } 
    // no need to break down, record op directly
    else {
      val operation = s"${opN}_+_${op_counter}"
      op_counter += 1
      if (tvalName == fvalName)
        updateOp(operation, Seq(tvalName -> 0), outName, op_bw)
      else
        updateOp(operation, Seq(condName -> 0, tvalName -> 1, fvalName -> 2), outName, op_bw)
    }
  }
  // operation extraction function for dynamic right shift
  def dshiftExtract(p: DoPrim, outName: String): Unit = {
    val bitWidth = TeAALExtractor.IntBitWidth(p.tpe).toInt
    val opN = (p.op, p.tpe) match {
      case (Dshr, _: UIntType)  => if (bitWidth <= 64) s"shr" else "dshr"
      case (Dshr, _: SIntType)  => if (bitWidth <= 64) s"shrS" else "dshrS"
      case (Dshl, _: UIntType)  => if (bitWidth <= 64) s"shl" else "dshl"
      case (Dshl, _: SIntType)  => if (bitWidth <= 64) s"shl" else "dshl"
      case _ => throw new Exception(s"Unsupported op/type combo: ${p.op}, ${p.tpe}")
    }

    val lhs = inOutMap.getOrElse(p.args(0).serialize, p.args(0).serialize)
    val rhs = inOutMap.getOrElse(p.args(1).serialize, p.args(1).serialize)
    extractMetadata(p.args(0).tpe, lhs, false)
    extractMetadata(p.args(1).tpe, rhs, false)
    extractMetadata(p.tpe, outName, false)

    val op_bw = math.max(bitWidthMetadata(outName), bitWidthMetadata(lhs))
    
    if (op_bw > 64) {
      val lhsChunks = ensureBreakdown(lhs, bitWidthMetadata(lhs))
      val outputsChunks = ensureBreakdown(outName, bitWidthMetadata(outName))
      wideDShift(opN, lhsChunks, rhs, outputsChunks, outName)
    } 
    else {
      updateOp(s"${opN}_+_${op_counter}", Seq(lhs -> 0, rhs -> 1), outName, bitWidthMetadata(outName))
      op_counter += 1
    }
  }

  def wideSInt(opN: String, left_opSet: HashMap[Int, String], opOut_set: HashMap[Int, String]): Unit = {
    assert(left_opSet.size == opOut_set.size)
    val last_index = left_opSet.size-1
    val bw = bitWidthMetadata(left_opSet(last_index))
    bitWidthMetadata((bw-1).toString) = 64 
    genTwoInOpOut(opN, left_opSet(last_index), (bw-1).toString, opOut_set(last_index), 64)
  }

  def wideUInt(opN: String, left_opSet: HashMap[Int, String], opOut_set: HashMap[Int, String]): Unit = {
    assert(left_opSet.size == opOut_set.size)
    val last_index = left_opSet.size-1
    val bw = bitWidthMetadata(left_opSet(last_index))
    val mask = ((BigInt(1) << bw) - 1).toString
    bitWidthMetadata(mask) = 64 
    genTwoInOpOut(opN, left_opSet(last_index), mask, opOut_set(last_index), bw)
  }

  // operation extraction function for type conversion ops, AsUInt, AsSInt, Cvt
  def tpeConvExtract(p: DoPrim, outName: String): Unit = {
    val input = inOutMap.getOrElse(p.args.head.serialize, p.args.head.serialize)
    extractMetadata(p.args.head.tpe, input, false)
    extractMetadata(p.tpe, outName, false)
    val input_bw = bitWidthMetadata(input)
    val input_blocks = ensureBreakdown(input, input_bw)
    
    p.op match {
      case AsUInt => {
        if (input_bw%64 != 0) {
          val opN = "and"
          
          if (input_bw > 64) {
            val outputsChunks = ensureBreakdown(outName, input_bw)
            wideUInt(opN, input_blocks, outputsChunks)
            val new_list = HashMap[Int, String]()
            for (ii <- 0 until input_blocks.size-1)
              new_list(ii) = input_blocks(ii)
            new_list(input_blocks.size-1) = outputsChunks(input_blocks.size-1)
            largeOpBreakDown(outName) =  new_list
          } 
          else {
            val mask = ((BigInt(1) << input_bw) - 1).toString
            bitWidthMetadata(mask) = 64 
            updateOp(s"${opN}_+_${op_counter}", Seq(input -> 0, mask->1), outName, input_bw)
            op_counter += 1
          }
        } else {
          inOutMap(outName) = input
          if (input_bw > 64)
            largeOpBreakDown.getOrElseUpdate(outName, input_blocks)
        }
      }
      case AsSInt => {
        if (input_bw%64 != 0) {
          val opN = "asSInt"
          
          if (input_bw > 64) {
            val outputsChunks = ensureBreakdown(outName, input_bw)
            wideSInt(opN, input_blocks, outputsChunks)
            val new_list = HashMap[Int, String]()
            for (ii <- 0 until input_blocks.size-1)
              new_list(ii) = input_blocks(ii)
            new_list(input_blocks.size-1) = outputsChunks(input_blocks.size-1)
            largeOpBreakDown(outName) =  new_list
          } 
          else {
            // val mask = ((BigInt(1) << input_bw) - 1).toString
            bitWidthMetadata((input_bw-1).toString) = 64 
            updateOp(s"${opN}_+_${op_counter}", Seq(input -> 0, (input_bw-1).toString->1), outName, input_bw)
            op_counter += 1
          }
        } else {
          inOutMap(outName) = input
          if (input_bw > 64)
            largeOpBreakDown.getOrElseUpdate(outName, input_blocks)
        }
      }
      case Cvt  => {
        if (input_bw%64 == 0 && p.args.head.tpe != p.tpe) { // this case bw must > 64
          val input_breakdown = input_blocks.clone()
          val output_breakdown = input_breakdown ++ HashMap((input_breakdown.keys.max + 1)->"0")
          bitWidthMetadata("0") = 64
          largeOpBreakDown.getOrElseUpdate(outName, output_breakdown)
        } else {
          inOutMap(outName) = input
          if (input_bw > 64)
            largeOpBreakDown.getOrElseUpdate(outName, input_blocks)
        }
      }
    }
  }

  // operation extraction function for pad and tail
  def padtailExtract(p: DoPrim, outName: String): Unit = {
    val input = inOutMap.getOrElse(p.args.head.serialize, p.args.head.serialize)
    extractMetadata(p.args.head.tpe, input, false)
    extractMetadata(p.tpe, outName, false)
    val input_bw = bitWidthMetadata(input)
    val output_bw = bitWidthMetadata(outName)
    val input_blocks = ensureBreakdown(input, input_bw)

    p.op match {
      case Pad => {
        p.args.head.tpe match {
          case UIntType(_) => {
            if ((input_bw+63)/64 != (output_bw+63)/64) { // this case output_bw must > 64
              val input_breakdown = ensureBreakdown(input, input_bw).clone()
              val n = (output_bw+63)/64 - (input_bw+63)/64
              val baseValue = "0"
              bitWidthMetadata("0") = 64
              val maxKey = input_breakdown.keys.maxOption.getOrElse(-1)
              val additions = (1 to n).map(i => (maxKey + i) -> baseValue).toMap
              val output_breakdown = input_breakdown ++ additions

              largeOpBreakDown.getOrElseUpdate(outName, output_breakdown)
            } else {
              inOutMap(outName) = input
              if (output_bw > 64)
                largeOpBreakDown.getOrElseUpdate(outName, input_blocks)
            }
          }
          case SIntType(_) => {
            val opN = "asSInt"

            if (output_bw <= 64) {
              bitWidthMetadata((input_bw-1).toString) = 64 
              updateOp(s"${opN}_+_${op_counter}", Seq(input -> 0, (input_bw-1).toString->1), outName, output_bw)
              op_counter += 1
            } else {
                val outputsChunks = ensureBreakdown(outName, output_bw)
                if ((input_bw+63)/64 == (output_bw+63)/64) {
                  wideSInt(opN, input_blocks, outputsChunks)
                  val new_list = HashMap[Int, String]()
                  for (ii <- 0 until input_blocks.size-1)
                    new_list(ii) = input_blocks(ii)
                  new_list(input_blocks.size-1) = outputsChunks(input_blocks.size-1)
                  largeOpBreakDown(outName) =  new_list
                } else {
                  val last_index = input_blocks.size-1
                  val bw = bitWidthMetadata(input_blocks(last_index))
                  bitWidthMetadata((bw-1).toString) = 64 
                  genTwoInOpOut(opN, input_blocks(last_index), (bw-1).toString, outputsChunks(last_index), 64)
                  val n = (output_bw+63)/64 - (input_bw+63)/64

                  val lt_output = genTwoInOp("ltS", outputsChunks(last_index), "0", bw)
                  for (ii <- last_index+1 to last_index+n) {
                    genThreeOp("mux", lt_output, "18446744073709551615", "0", outputsChunks(ii), 64)
                  }

                  val new_list = HashMap[Int, String]()
                  for (ii <- 0 until last_index)
                    new_list(ii) = input_blocks(ii)
                  for (ii <- last_index to last_index+n)
                    new_list(ii) = outputsChunks(ii)
                  largeOpBreakDown(outName) =  new_list
                }
            }
          }
        }
      }
      case Tail  => {
        val operand = inOutMap.getOrElse(p.args.head.serialize, p.args.head.serialize)
        extractMetadata((p.args.head).tpe, operand, false)
        extractMetadata(p.tpe, outName, false)
        val input_bw = bitWidthMetadata(operand)
        val output_bw = bitWidthMetadata(outName)
        val const = p.consts.head
        if (input_bw > 64) {
          ensureBreakdown(input, input_bw)
        }
        val input_breakdown = ensureBreakdown(input, input_bw).clone()
        val output_breakdown = HashMap[Int, String]()

        if ((input_bw+63)/64 != (output_bw+63)/64) {
          for (i <- 0 until (output_bw+63)/64) { // this assume tail const is always 1
            output_breakdown(i) = input_breakdown(i)
            if (output_bw==64) {
              inOutMap(outName) = input_breakdown(0)
            }
          }
        } else {
          if (input_bw <= 64) {
            val operation = s"and_+_${op_counter}"
            op_counter += 1
            val mask_len = output_bw
            val mask = (BigInt(1) << mask_len) - 1
            bitWidthMetadata(mask.toString) = mask_len+1 // +1?

            updateOp(operation, Seq(operand -> 0, mask.toString -> 1), outName, output_bw)
          } else {
            for (i <- 0 until ((output_bw+63)/64 - 1)) {
              output_breakdown(i) = input_breakdown(i)
            }

            val final_idx = (output_bw+63)/64 - 1
            val final_block = input_breakdown(final_idx)
            if (final_block == "0") {
              output_breakdown(final_idx) = "0"
            }
            else {
              val operation = s"and_+_${op_counter}"
              op_counter += 1
              val mask_len = output_bw%64
              val mask = (BigInt(1) << mask_len) - 1
              bitWidthMetadata(mask.toString) = mask_len+1

              updateOp(operation, Seq(final_block -> 0, mask.toString -> 1), s"${operation}_output", mask_len)
              output_breakdown(final_idx) = s"${operation}_output"
            }
          }
          largeOpBreakDown(outName) = output_breakdown
        }
      }
    }
  }

  // top function for assign->24
  def varUpdate(iEBuffer: BufferHashMap, oEBuffer: BufferHashMap, inEName: String, oEName: String, inTpe: Type, outTpe: Type): Unit = {
    val iEName = inOutMap.getOrElse(inEName, inEName) // inOut key and value may not have same bw, but bw/64 is the same!
    extractMetadata(inTpe, iEName, false)
    val op_bw = bitWidthMetadata(iEName)
    if (op_bw > 64) {
      extractMetadata(outTpe, oEName, false)
      val inputsChunks = ensureBreakdown(iEName, op_bw)
      val outputsChunks = ensureBreakdown(oEName, op_bw)
      wideAssign("assign", inputsChunks, outputsChunks)
    } else {
      val operation = s"assign_+_${op_counter}"
      bufferUpdate(iEBuffer, oEBuffer, iEName, oEName, operation)
      op_counter = op_counter + 1
      extractMetadata(outTpe, oEName, true, Some(operation))
    }
  }

  // ================================ PART 5: Actual Var Extraction ================================
  // recursive extraction function
  def extractVar(e: Expression): String = e match{
    case w: WRef => {
      extractMetadata(w.tpe, w.name, false)
      w.name
    }
    case u: UIntLiteral => {
      extractMetadata(u.tpe, u.serialize, false)
      u.serialize
    }
    case u: SIntLiteral => {
      extractMetadata(u.tpe, u.serialize, false)
      u.serialize
    }
    case m: Mux => {
      var nName = s"nestedNode${op_counter}"
      muxExtract(m, nName)
      nName
    }
    case w: WSubField => {
      val result = s"${extractVar(w.expr)}.${w.name}"
      extractMetadata(w.tpe, result, false)
      result
    }
    case p: DoPrim => {
      var nName = s"nestedNode${op_counter}"
      p.op match {
        case AsClock => throw new Exception("AsClock unimplemented!")
        // case AsAsyncReset  => {
        //   extractMetadata(p.args.head.tpe, p.args.head.serialize, false)
        //   p.args.head.serialize
        // }
        case AsUInt | AsSInt | Cvt  => {
          tpeConvExtract(p, nName)
          nName
        }
        case Pad | Tail => {
          padtailExtract(p, nName)
          nName
        }
        case Gt | Geq | Lt | Leq  => {
          compareExtract(p, nName)
          nName
        }
        case Shl => {
          shlExtract(p, nName)
          nName
        }
        case Shr => {
          shrExtract(p, nName)
          nName
        }
        case Dshr | Dshl => {
          dshiftExtract(p, nName)
          nName
        }
        case Add | Sub => {
          addsubExtract(p, nName)
          nName
        }
        case Cat => {
          catExtract(p, nName)
          nName
        }
        case Mul => {
          mulExtract(p, nName)
          nName
        }
        case Div => {
          divExtract(p, nName)
          nName
        }
        case Rem => {
          remExtract(p, nName)
          nName
        }
        case And | Or | Xor => {
          andorxorExtract(p, nName)
          nName
        }
        case Not => {
          notExtract(p, nName)
          nName
        }
        case Eq | Neq | Andr | Orr => {
          eqneqExtract(p, nName)
          nName
        }
        case Xorr => {
          xorrExtract(p, nName)
          nName
        }
        case Bits => {
          bitsExtract(p, nName)
          nName
        }
        case _ => {
          println("missing operation: ", p.op)
          "other\n"
        }
      }
    }
    case _ => {
      println("nested other\n")
      "other\n"
    }
  }

  // main data extraction function
  def extractStmt(stmt: Statement): Unit = stmt match {
    case b: Block => { b.stmts.foreach { stmt => 
            extractStmt(stmt)
    }}
    case d: DefNode => d.value match{
      case w: WRef => {
        varUpdate(inEdgeBuffer, outEdgeBuffer, w.name, d.name, w.tpe, d.value.tpe)
      }
      case u: UIntLiteral => {
        varUpdate(inEdgeBuffer, outEdgeBuffer, u.serialize, d.name, u.tpe, d.value.tpe)
      }
      case u: SIntLiteral => {
        varUpdate(inEdgeBuffer, outEdgeBuffer, u.serialize, d.name, u.tpe, d.value.tpe)
      }
      case m: Mux => {
        muxExtract(m, d.name)
      }
      case w: WSubField => {
        val result = s"${extractVar(w.expr)}.${w.name}"
        varUpdate(inEdgeBuffer, outEdgeBuffer, result, d.name, w.tpe, d.value.tpe)
      }
      case w: WSubAccess => { // memory read
        extractMetadata(d.value.tpe, d.name, false)
        val mem_arr_idx = extractVar(w.index).toString
        val memarr_bw = bitWidthMetadata(d.name)
        val wepr = extractVar(w.expr)
        val mem_arrays = ensureBreakdown(wepr, memarr_bw)
        val data_blocks = ensureBreakdown(d.name, memarr_bw)
        val addr_mask = ((BigInt(1) << bitWidthMetadata(mem_arr_idx)) - 1).toString
        bitWidthMetadata(addr_mask) = bitWidthMetadata(mem_arr_idx)

        mem_arrays foreach {case(idx, arr)=>{
          val arr_base_idx = memory_array(arr).toString
          val memory_block_idx = if (bitWidthMetadata(arr) > 8) "0" else "1"
          bitWidthMetadata(arr_base_idx) = 64
          updateOp(s"memr_+_${op_counter}", Seq(memory_block_idx->0, arr_base_idx -> 1, addr_mask -> 2, mem_arr_idx -> 3), data_blocks(idx), bitWidthMetadata(arr))
          op_counter += 1
        }}
      }
      case p: DoPrim => {
        p.op match {
          case AsClock => throw new Exception("AsClock unimplemented!")
          case AsAsyncReset  => {
            varUpdate(inEdgeBuffer, outEdgeBuffer, p.args.head.serialize, d.name, p.args.head.tpe, d.value.tpe)
          }
          case AsUInt | AsSInt | Cvt  => {
            tpeConvExtract(p, d.name)
          }
          case Pad | Tail => {
            padtailExtract(p, d.name)
          }
          case Gt | Geq | Lt | Leq  => {
            compareExtract(p, d.name)
          }
          case Shl => {
            shlExtract(p, d.name)
          }
          case Shr => {
            shrExtract(p, d.name)
          }
          case Dshr | Dshl => {
            dshiftExtract(p, d.name)
          }
          case Add | Sub => {
            addsubExtract(p, d.name)
          }
          case Cat => {
            catExtract(p, d.name)
          }
          case Mul => {
            mulExtract(p, d.name)
          }
          case Div => {
            divExtract(p, d.name)
          }
          case Rem => {
            remExtract(p, d.name)
          }
          case And | Or | Xor => {
            andorxorExtract(p, d.name)
          }
          case Not => {
            notExtract(p, d.name)
          }
          case Eq | Neq | Andr | Orr => {
            eqneqExtract(p, d.name)
          }
          case Xorr => {
            xorrExtract(p, d.name)
          }
          case Bits => {
            bitsExtract(p, d.name)
          }
          case _ => {
            println("missing operation: ", p.op)
          }
        }
      }
      case _ => {
        println("def other"+stmt.toString+"other\n")
      }
    }
    case c: Connect => c.expr match{
      case w: WRef => {
        varUpdate(inEdgeBuffer, outEdgeBuffer, w.name, c.loc.serialize, w.tpe, c.expr.tpe)
      }
      case u: UIntLiteral => {
        varUpdate(inEdgeBuffer, outEdgeBuffer, u.serialize, c.loc.serialize, u.tpe, c.expr.tpe)
      }
      case u: SIntLiteral => {
        varUpdate(inEdgeBuffer, outEdgeBuffer, u.serialize, c.loc.serialize, u.tpe, c.expr.tpe)
      }
      case m: Mux => {
        muxExtract(m, c.loc.serialize)
      }
      case w: WSubField => {
        val result = s"${extractVar(w.expr)}.${w.name}"
        varUpdate(inEdgeBuffer, outEdgeBuffer, result, extractVar(c.loc), w.tpe, c.expr.tpe)
      }
      case w: WSubAccess => {
        extractMetadata(c.expr.tpe, c.loc.serialize, false)
        val mem_arr_idx = extractVar(w.index).toString
        val memarr_bw = bitWidthMetadata(c.loc.serialize)
        val wepr = extractVar(w.expr)
        val mem_arrays = ensureBreakdown(wepr, memarr_bw)
        val data_blocks = ensureBreakdown(c.loc.serialize, memarr_bw)
        val addr_mask = ((BigInt(1) << bitWidthMetadata(mem_arr_idx)) - 1).toString
        bitWidthMetadata(addr_mask) = bitWidthMetadata(mem_arr_idx)

        mem_arrays foreach {case(idx, arr)=>{
          val arr_base_idx = memory_array(arr).toString
          val memory_block_idx = if (bitWidthMetadata(arr) > 8) "0" else "1"
          bitWidthMetadata(arr_base_idx) = 64
          updateOp(s"memr_+_${op_counter}", Seq(memory_block_idx->0, arr_base_idx -> 1, addr_mask -> 2, mem_arr_idx -> 3), data_blocks(idx), bitWidthMetadata(arr))
          op_counter += 1
        }}
      }
      case p: DoPrim => {
        p.op match {
          case AsClock => throw new Exception("AsClock unimplemented!")
          case AsAsyncReset  => {
            varUpdate(inEdgeBuffer, outEdgeBuffer, p.args.head.serialize, c.loc.serialize, p.args.head.tpe, c.expr.tpe)
          }
          case AsUInt | AsSInt | Cvt  => {
            tpeConvExtract(p, c.loc.serialize)
          }
          case Pad | Tail => {
            padtailExtract(p, c.loc.serialize)
          }
          case Gt | Geq | Lt | Leq  => {
            compareExtract(p, c.loc.serialize)
          }
          case Shl => {
            shlExtract(p, c.loc.serialize)
          }
          case Shr => {
            shrExtract(p, c.loc.serialize)
          }
          case Dshr | Dshl=> {
            dshiftExtract(p, c.loc.serialize)
          }
          case Add | Sub => {
            addsubExtract(p, c.loc.serialize)
          }
          case Cat => {
            catExtract(p, c.loc.serialize)
          }
          case Mul => {
            mulExtract(p, c.loc.serialize)
          }
          case Div => {
            divExtract(p, c.loc.serialize)
          }
          case Rem => {
            remExtract(p, c.loc.serialize)
          }
          case And | Or | Xor => {
            andorxorExtract(p, c.loc.serialize)
          }
          case Not => {
            notExtract(p, c.loc.serialize)
          }
          case Eq | Neq | Andr | Orr => {
            eqneqExtract(p, c.loc.serialize)
          }
          case Xorr => {
            xorrExtract(p, c.loc.serialize)
          }
          case Bits => {
            bitsExtract(p, c.loc.serialize)
          }
          case _ => {
            println("missing operation: ", p.op)
          }
        }
      }
      case _ => {
        println("connect other"+stmt.toString+"other\n")
      }
    }
    case p: Print => { /* No support for print at this point */ }
    case st: Stop => {}
    case mw: MemWrite => {
      val mwwrEn = extractVar(mw.wrEn)  // DoPrime
      val mwwrMask = extractVar(mw.wrMask)
      val mwwrData_ = extractVar(mw.wrData)
      val mwwrData = inOutMap.getOrElse(mwwrData_, mwwrData_)
      val mwwrAddr = extractVar(mw.wrAddr)
      val mem_port = s"${mw.memName}"
      extractMetadata(mw.wrData.tpe, mwwrData, false)
      val memarr_bw = bitWidthMetadata(mwwrData)
      bitWidthMetadata(mem_port) = memarr_bw

      val mem_arrays = ensureBreakdown(mem_port, memarr_bw)
      val data_blocks = ensureBreakdown(mwwrData, memarr_bw)

      val addr_mask = ((BigInt(1) << bitWidthMetadata(mwwrAddr)) - 1).toString
      bitWidthMetadata(addr_mask) = bitWidthMetadata(mwwrAddr)
      
      mem_arrays foreach {case(idx, arr)=>{
        val operation = s"memw_+_$op_counter"
        val arr_base_idx = memory_array(arr).toString
        val memory_block_idx = if (bitWidthMetadata(arr) > 8) "0" else "1"
        bitWidthMetadata(arr_base_idx) = 64
        val inputs = Seq(mwwrEn->0, mwwrMask->1, memory_block_idx->2, arr_base_idx->3, addr_mask->4, mwwrAddr->5, data_blocks(idx)->6)
        inputs.foreach { case (in, idx) =>
          inEdgeBuffer.getOrElseUpdate(in, HashSet.empty) += operation
          opInBufferOrder.getOrElseUpdate(operation, HashMap.empty).getOrElseUpdate(in, HashSet.empty) += idx
        }
        val tmp_output = s"${operation}_fake_output"
        outEdgeBuffer(tmp_output) = HashSet(operation)
        oEBufferR(operation) = tmp_output

        bitWidthMetadata(operation) =  bitWidthMetadata(arr)
        op_counter += 1
      }}
    }
    case ru: RegUpdate => {
      val reg_name = extractVar(ru.regRef)
      val regUp_name = s"${reg_name}@next"

      ru.expr match {
        case p: DoPrim => {
        p.op match {
          case AsClock => throw new Exception("AsClock unimplemented!")
          case AsAsyncReset  => {
            varUpdate(inEdgeBuffer, outEdgeBuffer, p.args.head.serialize, regUp_name, p.args.head.tpe, ru.expr.tpe)
          }
          case AsUInt | AsSInt | Cvt  => {
            tpeConvExtract(p, regUp_name)
          }
          case Pad | Tail => {
            padtailExtract(p, regUp_name)
          }
          case Gt | Geq | Lt | Leq  => {
            compareExtract(p, regUp_name)
          }
          case Shl => {
            shlExtract(p, regUp_name)
          }
          case Shr => {
            shrExtract(p, regUp_name)
          }
          case Dshr | Dshl=> {
            dshiftExtract(p, regUp_name)
          }
          case Add | Sub => {
            addsubExtract(p, regUp_name)
          }
          case Cat => {
            catExtract(p, regUp_name)
          }
          case Mul => {
            mulExtract(p, regUp_name)
          }
          case Div => {
            divExtract(p, regUp_name)
          }
          case Rem => {
            remExtract(p, regUp_name)
          }
          case And | Or | Xor => {
            andorxorExtract(p, regUp_name)
          }
          case Not => {
            notExtract(p, regUp_name)
          }
          case Eq | Neq | Andr | Orr => {
            eqneqExtract(p, regUp_name)
          }
          case Xorr => {
            xorrExtract(p, regUp_name)
          }
          case Bits => {
            bitsExtract(p, regUp_name)
          }
          case _ => {
            println("missing operation: ", p.op)
          }
        }
        }
        case m: Mux => {
          muxExtract(m, regUp_name)
        }
        case w: WRef => {
          varUpdate(inEdgeBuffer, outEdgeBuffer, w.name, regUp_name, w.tpe, ru.expr.tpe)
        }
        case w: WSubField => {
          val result = s"${extractVar(w.expr)}.${w.name}"
          varUpdate(inEdgeBuffer, outEdgeBuffer, result, regUp_name, w.tpe, ru.expr.tpe)
        }
        case w: WSubAccess => {
          extractMetadata(ru.expr.tpe, regUp_name, false)
          val mem_arr_idx = extractVar(w.index).toString
          val memarr_bw = bitWidthMetadata(regUp_name)
          val wepr = extractVar(w.expr)
          val mem_arrays = ensureBreakdown(wepr, memarr_bw)
          val data_blocks = ensureBreakdown(regUp_name, memarr_bw)
          println("data_blocks: ", data_blocks, regUp_name)
          val addr_mask = ((BigInt(1) << bitWidthMetadata(mem_arr_idx)) - 1).toString
          bitWidthMetadata(addr_mask) = bitWidthMetadata(mem_arr_idx)

          mem_arrays foreach {case(idx, arr)=>{
            val arr_base_idx = memory_array(arr).toString
            val memory_block_idx = if (bitWidthMetadata(arr) > 8) "0" else "1"
            bitWidthMetadata(arr_base_idx) = 64
            updateOp(s"memr_+_${op_counter}", Seq(memory_block_idx->0, arr_base_idx -> 1, addr_mask -> 2, mem_arr_idx -> 3), data_blocks(idx), bitWidthMetadata(arr))
            op_counter += 1
          }}
        }
        case u: UIntLiteral => {
          varUpdate(inEdgeBuffer, outEdgeBuffer, u.serialize, regUp_name, u.tpe, ru.expr.tpe)
        }
        case u: SIntLiteral => {
          varUpdate(inEdgeBuffer, outEdgeBuffer, u.serialize, regUp_name, u.tpe, ru.expr.tpe)
        }
        case _ => {
          println("weird regup: "+stmt.toString+" \n")
        }
      }

      if (!bitWidthMetadata.contains(reg_name)) {
        bitWidthMetadata(reg_name) = bitWidthMetadata(regUp_name)
      }
      regOrder ++= ensureBreakdown(reg_name, bitWidthMetadata(reg_name)).values
      regNextOrder ++= ensureBreakdown(regUp_name, bitWidthMetadata(regUp_name)).values
    }
    case r: DefRegister => {
      println(stmt.toString+"def reg\n")
    }
    case r: CDefMPort => {
      println(stmt.toString+"CDefMPort\n")
    }
    case r: DefWire => {
      println(stmt.toString+"DefWire\n")
    }
    case _ => {
      println("just other: "+stmt.toString+ " other\n")
    } 
  }

  def mem_reg_update(): HashMap[String, HashSet[Int]] = {
    var extra_mem_inputs = HashMap[String, HashSet[Int]]()
    opInBufferOrder foreach {case(op, inputs)=>{
      if (op.startsWith("memw")) {
        val memory_block_idx = ju.inputSearch(inputs, 2)
        val arr_base_idx = ju.inputSearch(inputs, 3)
        val all_in_ops = inEdgeBuffer(memory_block_idx) & inEdgeBuffer(arr_base_idx)
        val mem_r = all_in_ops.filter(_.startsWith("memr"))
        val input_mem_r = HashSet[String]()
        mem_r foreach {memr=>{
          val mem_r_input = opInBufferOrder(memr)
          val mem_r_block_idx = ju.inputSearch(mem_r_input, 0)
          val mem_r_base_idx = ju.inputSearch(mem_r_input, 1)
          if ((mem_r_block_idx == memory_block_idx) && (mem_r_base_idx == arr_base_idx)) {
            input_mem_r += oEBufferR(memr)
          }
        }}

        var idx = 7
        var idx_arr = HashSet[Int]()
        input_mem_r foreach {dependent_memr=>{
          inputs.getOrElseUpdate(dependent_memr, HashSet.empty) += idx
          idx_arr += idx
          inEdgeBuffer.getOrElseUpdate(dependent_memr, HashSet.empty) += op
          idx += 1
        }}
        if (idx_arr.nonEmpty) {
          extra_mem_inputs(op) = idx_arr
        }
      }
    }}
    return extra_mem_inputs
  }
}

object TeAALExtractor {
  type BufferHashMap = HashMap[String,HashSet[String]]
  type opInOrder = HashMap[String, HashMap[String, HashSet[Int]]]

  // extract bit width from iterals 
  def IntBitWidth(tpe: Type, key: Option[String] = None, operation: Option[String] = None): Int = {
    val pattern = """<(\d+)>""".r
    var bitWidth = 0
    tpe match {
      case u: UIntType => {
        bitWidth = pattern.findFirstMatchIn(u.width.serialize).map(_.group(1).toInt).getOrElse(0)
      }
      case s: SIntType => {
        bitWidth = pattern.findFirstMatchIn(s.width.serialize).map(_.group(1).toInt).getOrElse(0)
      }
      case AsyncResetType => {
        bitWidth = 1
      }
      case ResetType => {
        bitWidth = 1
      }
      case _ => {}
    }
    bitWidth
  }
}