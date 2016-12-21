

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
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.LinkedBlockingQueue

object Day11threaded {

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
    // pairs += Pair(1,0)
    // pairs += Pair(2,0)

    // pairs += Pair( 0, 0 )
    // pairs += Pair( 1, 2 )
    // pairs += Pair( 1, 2 )
    // pairs += Pair( 1, 2 )
    // pairs += Pair( 1, 2 )
    
    // 2
    pairs += Pair( 0, 0 )
    pairs += Pair( 1, 2 )
    pairs += Pair( 1, 2 )
    pairs += Pair( 1, 2 )
    pairs += Pair( 1, 2 )
    pairs += Pair( 0, 0 )
    pairs += Pair( 0, 0 )

    val start = State( 0, pairs.sortWith(compare).toList, List() ) 
    print( start )

    Console.println( isSafe( start ) )
    Console.println( stateToString( start ) )
    Console.println( stateToLong( start ) )
    
    // val ele = elevatorFloor( start )
    // Console.println( ele )
    // Console.println( generateMoves( List( 1, 1, 1, 0 ), start.pairs.size ) )
    
    solve(start)
    
    if( shortest.isDefined ){
      print( shortest.get )
      Console.println( shortest.get.states.size )
      printSolution( shortest.get )
      Console.println( shortest.get.states.size )
    }
    
  }
  
  def printSolution( state : State ) = {
    
    for( s <- state.states ){
      print( visited.get(s) )
    }
    
    print(state)
    
  }
  
  val solutions = ListBuffer[State]()
  var shortest : Option[State] = None
  
  def solved( state : State ) = {
    
    solutions += state
    
    if( shortest.isDefined ){
      if( state.states.size < shortest.get.states.size ){
        shortest = Some(state)
      }
    }
    else {
      shortest = Some(state)
    }
    
  }
  
  def isLoop( start : State ) : Boolean = {
    start.states.contains( stateToLong( start ) )
  }
  
  val visited = new java.util.concurrent.ConcurrentHashMap[Long,State]()
  
  def isVisited( state : State ) : Boolean = {
    visited.contains( stateToLong(state) )
  }
  
  def visit( state : State ) = {
    visited.put(stateToLong(state),state)
  }
  
  def isTooLong( state : State ) : Boolean = {
    if( shortest.isDefined ){
      state.states.size >= shortest.get.states.size
    }
    else {
      false
    }
  }
  
  var steps = 0
  
  class Solver( val current : State ) extends Runnable {
    
    override def run() : Unit = {
      
      checked = checked + 1
      
      // Console.println("solve...")
      
      if( isSolution( current ) ){
        Console.println("solved...")
        solved(current)
      }
      else {
        
        visit(current)
        
        steps = current.states.size
        
        val ele = elevatorFloor(current)
        val mvs = generateMoves(ele, current.pairs.size )
      
        // for each move
        for( mv <- mvs ){
          up( current, mv ) match {
            case Some(up) => {
              if( isVisited(up) ){
                //
                checked = checked + 1
              }
              else if( isLoop(up) ) {
                visit(up)
                checked = checked + 1
              }
              else if( isTooLong(up) ){
                //
                visit(up)
                checked = checked + 1
              }
              else if( !isSafe(up) ) {
                visit(up)
                checked = checked + 1
              }
              else {
                visit(up)
                pool.execute(new Solver(up))
              }
            }
            case None => {
                checked = checked + 1
            }
          }
          down( current, mv ) match {
            case Some(down) => {
              if( isVisited(down) ){
                //
              }
              else if( isLoop(down) ) {
                visit(down)
              }
              else if( isTooLong(down) ){
                //
                visit(down)
              }
              else if( !isSafe(down) ) {
                visit(down)
              }
              else {
                visit(down)
                pool.execute(new Solver(down))
              }
            }
            case None => {
                checked = checked + 1
            }
          }
        }
      
      }
    }
      
  }
  
  val work = new LinkedBlockingQueue[Runnable](1000000)
  val pool = new ThreadPoolExecutor( 100, 2000, 2, TimeUnit.SECONDS, work ) 
  var checked = 0L

  def solve( start : State ) = {
    
    var lo = 0
    var hi = 0
    
    pool.execute(new Solver(start))
    try{
       Thread.sleep(10000)
    }
    catch{ 
      case t : Throwable => {
        t.printStackTrace()
      }
    }
    
    while( pool.getActiveCount > 0 ){

      try{
       Thread.sleep(10000)
      }
      catch{ 
        case t : Throwable => {
          t.printStackTrace()
        }
      }
      
      checked = checked + 1
      
      Console.println( "Pool..." )
      Console.println( "A:"+ pool.getActiveCount )
      Console.println( "C:"+ pool.getCompletedTaskCount )
      Console.println( "Q:"+ pool.getQueue.size() )
      Console.println( "C:"+ checked )
      Console.println( "V:"+ visited.size() )
      Console.println( "S:"+ solutions.size )
      Console.println( "S:"+ steps )
     
    }
    
    try{
      //
      pool.shutdown()
    }
    catch {
      case t : Throwable => {
        t.printStackTrace()
      }
    }
    
  }
  
  def up( state : State, is : List[Int] ) : Option[State] = {
    if( state.ele >= ((state.pairs.size * 2) - 1) ) {
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
  
  def isSolution( state : State ) : Boolean = {
    state.pairs.forall( (p:Pair) => { p.chip == 3 && p.rtg == 3 } )
  }
  
  def print( state : State ) = {
    Console.println( "-" + isSolution( state ) )
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