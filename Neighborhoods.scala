import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Neighborhoods extends App {
  // simple Manhattan Distance function that just tells us if it's within n steps
  def withinMD(c1: (Int, Int), c2: (Int, Int), n: Int): Boolean = {
    ((c1._1-c2._1).abs + (c1._2-c2._2).abs) <= n
  }

  // our do the work function
  def numNeighbors(filepath: String): Int = {
    // pull in our input to parse first of course
    val fileiter = Source.fromFile(filepath).getLines
      
    // grab h, w, n from first line, totally assuming well-formed input for everything
    val Array(h, w, n) = fileiter.next.split(" ").map(_.toInt)    

    // loop to find positive cells, assuming values given in order by row: 0,0...0,h-1...r-1,h-1     
    // also go ahead and create our all-coordinates list here
    // if we wanted to start caring about values more specifically, can make a map/2D list here
    val (allcells, poscells) = (ArrayBuffer[(Int, Int)](), ArrayBuffer[(Int, Int)]())
    for (row <- 0 until h) {
      for (col <- 0 until w) {
        val cell = (row, col)
        allcells.append(cell)
        if (fileiter.next.toInt > 0) poscells.append(cell)
      }
    }
    
    // need to find subset of allcells whose members C satisfy V[c in C] E[p in poscells] st. MD(m, p) <= n
    // in this case we just want this set's cardinality, which is the count of (unique) neighborhood cells
    var neighborcount = 0
    allcells.foreach(c => if (poscells.exists(withinMD(_, c, n))) neighborcount+=1)
    
    neighborcount
  }
  
  // ok go
  if (args.size == 0) {
    println("No input provided")
  } else {
    println(s"# of cells in neighborhoods: ${numNeighbors(args(0))}")        
  }    
}
