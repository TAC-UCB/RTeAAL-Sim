package essent

import ujson._
import scala.io.Source
import io.circe.syntax._
import io.circe.{Encoder, Json}
import java.io.PrintWriter
import scala.util.Random
import scala.jdk.CollectionConverters._
import java.nio.file.{Paths, Files}
import collection.mutable.{HashMap, LinkedHashMap, HashSet, ArrayBuffer}


class TeAALUtil {
  val decimalPattern = """^-?\d+$""".r
  val Upattern = raw"""UInt<\d+>\("h([0-9a-fA-F]+)"\)""".r
  val Spattern = raw"""SInt<\d+>\("h(-?[0-9a-fA-F]+)"\)""".r
  val UIntBitWidth = raw"UInt<(\d+)>.*".r
  val SIntBitWidth = raw"SInt<(\d+)>.*".r
  var memory_array = LinkedHashMap[String, Int]()
  var memory_array_len = HashMap[String, Int]()


  def writeToJsonFile[T](filename: String, data: Value): Unit = {
    val writer = new PrintWriter(filename)
    writer.write(data.render(indent = 2))
    writer.close()
  }

  def memoryIdxRead(filename: String): Unit = {
    val source = Source.fromFile(filename)
    val jsonString = try source.mkString finally source.close()
    val parsedJson = ujson.read(jsonString)
    for ((k, v) <- parsedJson.obj) {
      memory_array(k) = v.num.toInt
    }
  }

  def memoryLengthRead(filename: String): Unit = {
    val source = Source.fromFile(filename)
    val jsonString = try source.mkString finally source.close()
    val parsedJson = ujson.read(jsonString)
    for ((k, v) <- parsedJson.obj) {
      memory_array_len(k) = v.num.toInt
    }
  }

  def saveToJsonFile(data: HashMap[String, HashSet[String]], filename: String): Unit = {
    val json: Json = data.asJson
    val writer = new PrintWriter(filename)
    writer.write(json.spaces2)
    writer.close()
  }

  def readJumpTableSet(path: String): HashSet[Int] = {
    val content = Files.readAllLines(Paths.get(path))
      .asScala
      .mkString
      .trim
    val set = HashSet[Int]()
    content.split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
      .foreach { x =>
        set += x.toInt
      }
    set
  }

  def readJsonMapUjson(path: String): HashMap[String, Int] = {
    val source = Source.fromFile(path)
    val jsonStr = try source.mkString finally source.close()
    val json    = ujson.read(jsonStr)

    val out = HashMap[String, Int]()
    for ((k, v) <- json.obj) {
      out += k -> v.num.toInt
    }
    out
  }

  def readJsonMapArray(path: String): HashMap[String, ArrayBuffer[Int]] = {
    val source = Source.fromFile(path)
    val jsonStr = try source.mkString finally source.close()
    val json = ujson.read(jsonStr)

    val out = HashMap[String, ArrayBuffer[Int]]()

    json.obj.foreach { case (k, v) =>
      val arr = ArrayBuffer[Int]()
      v.arr.foreach { x =>
        arr += x.num.toInt
      }
      out += k -> arr
    }

    out
  }

  def isInte(iEName: String): Boolean = {
    if ((iEName.startsWith("UInt<") && iEName.contains(">(")) || (iEName.startsWith("SInt<") && iEName.contains(">("))) true
    else false
  }
  
  def hexInt(s: String): BigInt = s match {
    case Upattern(hexStr) => BigInt(hexStr, 16)
    // case Spattern(hexStr) => BigInt(String.format("%016x", java.lang.Long.parseLong(hexStr, 16)), 16)
    // case Spattern(hexStr) =>
    //   // Handle the sign manually
    //   if (hexStr.startsWith("-")) {
    //     -BigInt(hexStr.drop(1), 16)
    //   } else {
    //     BigInt(hexStr, 16)
    //   }
    case Spattern(hexStr) => {
      val width = 64
      val value = if (hexStr.startsWith("-")) {
        // Compute two's complement: 2^width + value
        val absVal = BigInt(hexStr.drop(1), 16)
        (BigInt(1) << width) - absVal
      } else {
        BigInt(hexStr, 16)
      }
      value
    }
    case _ => {
    println(s)
    throw new IllegalArgumentException("Invalid format")
    }
  }

  def signExtend(value: BigInt, width: Int): BigInt = {
    val signBit = BigInt(1) << (width - 1)
    val signed = (value ^ signBit) - signBit
    signed.abs
  }

  def stringInt(s: String): BigInt = {
    if (isInte(s)) 
      return hexInt(s)
    else if (isDecimalInt(s))
      return BigInt(s)
    else {
      println(s)
      throw new IllegalArgumentException("Invalid format")
    }
  }

  def randUInt(bits: Int): BigInt = {
    require(bits <= 64, "UInt bits must be <= 64 to fit in Long")
    val max = BigInt(1) << bits - 1
    (BigInt(bits, Random) % max)
  }

  def randSInt(bits: Int): BigInt = {
    require(bits <= 64, "SInt bits must be <= 64 to fit in Long")
    val min = -(BigInt(1) << (bits - 1))
    val max = (BigInt(1) << (bits - 1)) - 1
    val range = max - min + 1
    (min + (BigInt(bits + 1, Random) % range))
  }

  def randValue(bits: Int): BigInt = {
    randUInt(bits)
  }

  def isDecimalInt(s: String): Boolean = {
    decimalPattern.matches(s)
  }

  def toTwoComp64(value: BigInt): BigInt = {
    if (value >= 0) value
    else (BigInt(1) << 64) + value
  }

  def formatAsUIntPattern(value: BigInt, bitWidth: Int): String = {
    val hexStr = value.toString(16)
    s"""UInt<$bitWidth>("h$hexStr")"""
  }

  def formatAsSIntPattern(value: BigInt, bitWidth: Int): String = {
    val hexStr = value.toString(16)
    s"""SInt<$bitWidth>("h$hexStr")"""
  }

  def IntBWExtract(numberString: String): Int = {
    numberString match {
      case UIntBitWidth(bits) => bits.toInt
      case SIntBitWidth(bits) => bits.toInt
      case _ =>
        println(s"[Error] No valid UInt/SInt pattern found. Got value:")
        println(numberString)
        throw new RuntimeException("No valid 'Number' format in input.")
    }
  }

  def IntBitWidthExtract(ops: HashMap[Int, HashMap[String, String]]): Int = {
    val numberStrings = ops.values.flatMap(_.get("Number")).toSeq
    numberStrings.collectFirst {
      case UIntBitWidth(bits) => bits.toInt
      case SIntBitWidth(bits) => bits.toInt
    }.getOrElse {
      println(s"[Error] No valid UInt/SInt pattern found. Got values:")
      numberStrings.foreach(v => println(s"  - $v"))
      throw new RuntimeException("No valid 'Number' format in input.")
    }
  }

  def inputSearch(inputs: HashMap[String, HashSet[Int]], idx: Int): String = {
    inputs foreach {case(input, indices)=>{
      if (indices.contains(idx)) {
        return input
      }
    }}
    throw new NoSuchElementException(s"Operand with index $idx not found")
  }

  def inputIdxSearch(inputs: HashMap[String, HashSet[Int]], in_string: String): HashSet[Int] = {
    inputs foreach {case(input, indices)=>{
      if (input == in_string) {
        return indices
      }
    }}
    throw new NoSuchElementException(s"Operand with input string $in_string not found")
  }

  def operationStat(opName: String, opInBufferOrder: HashMap[String, HashMap[String, HashSet[Int]]]): Int = {
    var ret = 0
    opInBufferOrder foreach { case(name, ins)=>{
      val opType = name.split("_\\+_")(0)
      if (opType == opName) {
        ret += 1
      }
    }}
    ret
  }

  def operationList(opName: String, opInBufferOrder: HashMap[String, HashMap[String, HashSet[Int]]]): HashSet[String] = {
    var ret = HashSet[String]()
    opInBufferOrder foreach { case(name, ins)=>{
      val opType = name.split("_\\+_")(0)
      if (opType == opName) {
        ret += name
      }
    }}
    ret
  }
}

object TeAALUtil {}