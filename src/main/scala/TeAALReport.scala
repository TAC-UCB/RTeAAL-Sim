package essent

import essent.TeAALUtil._
import essent.TeAALExtractor._
import collection.mutable.{ArrayBuffer, HashMap, HashSet}

class TeAALReport(op_counter: Int, uniqueOps: Int, LAYER_NUM: Int, maxL: Int, TOTAL_IOP: Int, 
                  iEBuffer: BufferHashMap, oEBuffer: BufferHashMap, inNeigh: BufferHashMap, outNeigh: BufferHashMap) {


    import TeAALExtractor.{BufferHashMap}


    def edgeChecker(iNeigh: BufferHashMap, oNeigh: BufferHashMap): Int = {
      var inEdge = 0
      var outEdge = 0
      iNeigh foreach (
        kv => inEdge = inEdge + kv._2.size
      )
      outNeigh foreach (
        kv => outEdge = outEdge + kv._2.size
      )
      if (inEdge != outEdge) throw new Exception("Edge Number Calculation Fail!")
      inEdge
    }
  

    def iOEdge(EBuffer: BufferHashMap): Int = {
      var edgeCount = 0
      EBuffer foreach (
        io => edgeCount = edgeCount + io._2.size
      )
      edgeCount
    }


    def regCheck(regOrder: HashSet[String], layerMap: BufferHashMap): Unit = {
      regOrder  foreach {reg => {
        if (!layerMap("input").contains(reg)) {
          println(reg)
          throw new Exception("Register check fail!")
        }
      }}
    }


    def regDebug(regName: String, layerMap: BufferHashMap, oEBufferR: HashMap[String, String]): Unit = {
    layerMap foreach {case (layer, ops)=>{
      ops foreach {op=>{
        if (oEBufferR.contains(op)) {
          val output = oEBufferR(op)
          if (output == s"${regName}") {
            println(s"find reg update : ${regName}, op is: ${op}, at layer: ${layer}\n")
          }
        }
      }}
    }}
  }
  

    def DFGraph_Report(): Unit = {

      val edgeCheck = edgeChecker(inNeigh, outNeigh)
      val inputEdge = iOEdge(iEBuffer)
      val outputEdge = iOEdge(oEBuffer)

      println("\n ============================== DFGraph Info ============================== \n\n")
      println(s"Number of Operations: ${op_counter}  \n")
      println(s"Number of Unique Operations: $uniqueOps \n")
      println(s"Number of Inputs: ${iEBuffer.size} Number of Outputs: ${oEBuffer.size} \n")
      println(s"Number of Operation Edges: $edgeCheck\n")
      println(s"Number of Edges Connected to Inputs: $inputEdge \n")
      println(s"Number of Edges Connected to Outputs: $outputEdge \n")
      println(s"Number of Layers: $LAYER_NUM \n")
      println(s"Largest Layer: $maxL \n")
      println(s"Number of Added Identity Operations: $TOTAL_IOP\n")
      println(" ============================== End of Report ============================== \n")
    }

}


object TeAALReport {}