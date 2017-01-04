

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import java.util.Date
import scala.collection.mutable.PriorityQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.PriorityBlockingQueue
import java.util.Comparator

object Day11pairs4 {

  // val elements = List( 'H', 'L' )
  // val elements = List( 'P', 'C', 'U', 'R', 'L' )

  case class State( val ele : Int, pairs : List[Pair], states : List[Long] ) 
  
  
  def floorToString( floor : Int, state : State ) : String = {
    
    var f = ""
    
    if( floor == state.ele ){
      f = f + "1"
    }
    else{
      f = f + "0"
    }
    
    // loop through the pairs
    for( p <- state.pairs ){
      
      if( floor == p.rtg ){
        f = f +"1"
      }
      else {
        f = f +"0"
      }

      if( floor == p.chip ){
        f = f +"1"
      }
      else {
        f = f +"0"
      }
    }
    
    f
    
  }
  
  def stateToString( state : State ) : String = {
    // build a string
    
    var s = "" 
    
    for( f <- 0 until 4 ){
      s = s + floorToString( f, state)
    }
    
    s
  }
  
  def stateToLong( state : State ) : Long = {
    java.lang.Long.parseLong(stateToString(state), 2)
  }
  
  case class Pair( rtg : Int, chip : Int )

  def main( args : Array[String] ) : Unit = {

    Console.println( "day11..." )

    /*
     *                G  M  G  M  G  M  G  M  G  M
    val f4 = List( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
    val f3 = List( 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 )
    val f2 = List( 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0 )
    val f1 = List( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 )
    */

    /*
    val f4 = List( 0, 0, 0, 0, 0 )
    val f3 = List( 0, 0, 0, 1, 0 )
    val f2 = List( 0, 1, 0, 0, 0 )
    val f1 = List( 1, 0, 1, 0, 1 )
    */
    
    val pairs = ListBuffer[Pair]()
    // 1
     pairs += Pair(1,0)
     pairs += Pair(2,0)

    // j
      // pairs += Pair( 0, 0 )
      // pairs += Pair( 1, 2 )
      // pairs += Pair( 1, 2 )
      // pairs += Pair( 1, 2 )
      // pairs += Pair( 1, 2 )
    
    // 2
     // pairs += Pair( 0, 0 )
     // pairs += Pair( 1, 2 )
     // pairs += Pair( 1, 2 )
     // pairs += Pair( 1, 2 )
     // pairs += Pair( 1, 2 )
     // pairs += Pair( 0, 0 )
     // pairs += Pair( 0, 0 )

    val start = State( 0, pairs.sortWith(compare).toList, List() ) 
    print( start )

    Console.println( isSafe( start ) )
    Console.println( stateToString( start ) )
    Console.println( stateToLong( start ) )
    
    // val ele = elevatorFloor( start )
    // Console.println( ele )
    // Console.println( generateMoves( List( 1, 1, 1, 0 ), start.pairs.size ) )
    
    // solve(start)
    
    if( shortest.isDefined ){
      print( shortest.get )
      Console.println( shortest.get.states.size )
      printSolution( shortest.get )
      Console.println( shortest.get.states.size )
    }
    
  }
  
  def printSolution( state : State ) = {
    
    for( s <- state.states ){
      print( visited.get(s).get )
    }
    
    print(state)
    
  }
  
  var shortest : Option[State] = None

  def isSolution( state : State ) : Boolean = {
    if( state.pairs.forall( (p:Pair) => { p.chip == 3 && p.rtg == 3 } ) ){
      
      if( shortest.isDefined ){
        if( state.states.size < shortest.get.states.size ){
          shortest = Some(state)
        }
      }
      else {
        shortest = Some(state)
      }
      
      true
    }
    else {
      false
    }
  }

  def isLoop( start : State ) : Boolean = {
    start.states.contains( stateToLong( start ) )
  }
  
  var visited = HashMap[Long,State]()
  
  def isVisited( state : State ) : Boolean = {
    visited.get( stateToLong(state) ) match {
      case Some(s) => { 
        
        if( state.states.size < s.states.size ){
          visited.put( stateToLong(state), state )
          false
        }
        else {
          // the current path is shorter
          true
        }
        
      }
      case None => { 
        visited.put( stateToLong(state), state )
        false 
      }
    }
  }
  
  def isTooLong( state : State ) : Boolean = {
    if( shortest.isDefined ){
      state.states.size >= shortest.get.states.size
    }
    else {
      false
    }
  }
  
  val ds = List(1,3,7,11)
  
  def distance( state : State ) : Int = {
    
    var sum = ds(state.ele)
    
    for( p <- state.pairs ){
      sum += ds(p.chip)
      sum += ds(p.rtg)
    }
    
    sum
    
  }
  
  /*
  class ByDistance extends Comparator[SolverTask] {
      override def compare( s1 : SolverTask, s2 : SolverTask ) : Int = {
        val d1 = distance(s1.state)
        val d2 = distance(s2.state)
        d1.compare(d2)
      }
  }

  val work = new PriorityBlockingQueue[SolverTask]( 1000, new ByDistance() )
  // val solverExecutor = new ThreadPoolExecutor( 100, 2000, 10, TimeUnit.SECONDS, work )

  def solve( start : State ) = {
    
    
    isVisited(start)
    solverExecutor.execute( new SolverTask( start ) )
    
    
    while( solverExecutor.getActiveCount() > 0 ) {
      
      Console.println(".")
      
      try{
        Thread.sleep(10000)
      }
      catch{
        case t : Throwable => {
        }
      }

    }
    
    solverExecutor.shutdown()
      
    
  }
  * 
  
  class SolverTask( val state : State ) extends Runnable {
    
    override def run() : Unit = {

      // is this safe?
      if( isSolution( state ) ){
        Console.println("solved...")
        print(state)
      }
      else {
        
        val ele = elevatorFloor(state)
        val mvs = generateMoves(ele, state.pairs.size )
      
        // for each move
        for( mv <- mvs ){

          up( state, mv ) match {
            case Some(up) => {
              if( isVisited(up) ){
                // done
              }
              else if( isLoop( up ) ){
                // done
              }
              else if( isTooLong( up ) ){
                // done
              }
              else if( !isSafe(up) ){
                // done
              }
              else {
                solverExecutor.execute(new SolverTask(up))
              }
            }
            case None => {
              // done 
            }
          }

          down( state, mv ) match {
            case Some(down) => {
              if( isVisited(down) ){
                // done
              }
              else if( isLoop( down ) ){
                // done 
              }
              else if( isTooLong( down ) ){
                // done 
              }
              else if( !isSafe(down) ){
                // done 
              }
              else {
                solverExecutor.execute(new SolverTask(down))
              }
            }
            case None => {
                // done 
            }
          }
        }

      }
      
    }
  }
  */
  
  def up( state : State, is : List[Int] ) : Option[State] = {
    if( state.ele >= 3 ) {
      None
    }
    else {
      Some(apply( state, state.ele + 1, is ))
    }
  }
  
  def down( state : State, is : List[Int] ) : Option[State]= {
    if( state.ele == 0 ){
      None
    }
    else {
      val l = is.map( _ * -1 )
      Some(apply( state, state.ele - 1, l ))
    }
  }
  
  def apply( state : State, ele : Int, is : List[Int] ) : State = {
    
    // 
    val pairs = ListBuffer[Pair]()
    
    for( i <- 0 until state.pairs.size ){
      
      val rtg = if( state.ele == state.pairs(i).rtg ){
        state.pairs(i).rtg + is(i*2)
      }
      else {
        state.pairs(i).rtg
      }

      val chip = if( state.ele == state.pairs(i).chip ){
        state.pairs(i).chip + is((i*2) + 1)
      }
      else {
        state.pairs(i).chip
      }
      
      pairs += Pair( rtg, chip )
    }
    
    // create a new state 
    State( ele, pairs.toList.sortWith(equals), state.states :+ stateToLong(state) )  
  }

  def elevatorFloor( state : State ) : List[Int] = {
    val ps = ListBuffer[Int]()
    
    for( p <- state.pairs ){
      if( state.ele == p.rtg ){
        ps += 1 
      }
      else {
        ps += 0
      }
      if( state.ele == p.chip ){
        ps += 1 
      }
      else {
        ps += 0
      }
    }
    
    ps.toList
  }
  
  def isSafe( state : State ) : Boolean = {
    
    val unprotected = for( p <- state.pairs if p.chip != p.rtg ) yield p 
    
    // for each unprotected chip - is there a gen on that floor
    if( unprotected.size > 0 ){
      
      !unprotected.exists( (p1:Pair) => {
        state.pairs.exists( (p2:Pair) => { p2.rtg == p1.chip } )
      })
      
    }
    else {
      true
    }
  }
  
  def greater( s1 : State, s2 : State ) : Boolean = {
    
    // sum s1
    val sum1 = s1.pairs.foldLeft(s1.ele)( (a:Int,b:Pair) => { a + b.rtg + b.chip } )
    val sum2 = s2.pairs.foldLeft(s2.ele)( (a:Int,b:Pair) => { a + b.rtg + b.chip } )
    
    sum1 > sum2
  }
  
  def equals( s1 : State, s2 : State ) : Boolean = {
    if( s1.ele == s2.ele ){
      
      val z = s1.pairs.zip( s2.pairs )
      z.forall( (t:(Pair,Pair)) => { equals( t._1, t._2 ) } )
    }
    else {
      false
    }
  }
  
  def equals( p1 : Pair, p2 : Pair ) : Boolean = {
    p1.rtg == p2.rtg && p1.chip == p2.chip
  }
  
  def compare( p1 : Pair, p2 : Pair ) : Boolean = {
    
    if( p1.rtg > p2.rtg ) {
      true
    }
    else {
      p1.chip > p2.chip
    }
  }
  
  
  def print( state : State ) = {
    Console.println( "-" + isSolution( state ) )
    Console.println( state.states.size )
    for( i <- 3 to 0 by -1 ){
      
      if( state.ele == i ){
        Console.print( "E " )
      }
      else {
        Console.print( ". " )
      }
      
      for( p <- state.pairs ){
        
        if( p.rtg == i ){
          Console.print( "G " )
        }
        else {
          Console.print( ". " )
        }
        
        if( p.chip == i ){
          Console.print( "C " )
        }
        else {
          Console.print( ". " )
        }
      }
      
      Console.print('\n')
      
    }
    Console.println( "-" )
  }
  
  
  def generateMoves( floor : List[Int],  noOfPairs : Int ) : List[List[Int]] = {

    val all = for ( i <- 0 until ( BigInt( 2 ) ).pow( noOfPairs * 2 ).toInt ) yield {
      val s = i.toBinaryString.reverse
      val p = s.padTo( noOfPairs * 2, '0' )
      p.reverse
    }
    
    val validCount = all.filter( ( s : String ) => {
      val ones = s.filter( ( c : Char ) => { c == '1' } )
      ones.size <= 2 && ones.size > 0
    } )

    val l = validCount.map( ( s : String ) => {
      val t = s.map( ( c : Char ) => { c.toInt - 48 } )
      t.toList
    } )
    
    
    // screen by floor
    val validForFloor = l.filter( (is:List[Int]) => {
      var ok = true
      for( i <- 0 until is.size ){
        if( is(i) == 1 && floor(i) == 0 ){
          ok = false
        }
      }
      ok
    })

    validForFloor.toList

  }


}