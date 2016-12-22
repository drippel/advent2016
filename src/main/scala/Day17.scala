

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

object Day17 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day17...")
    
    // 0 to 3
    // 0 to 3
    val h = calcHash(code)
    Console.println(h)
    val mvs = calcOpen(h)
    Console.println(mvs)
    
    solve( new Step( 0, 0, Start(), List() ) ) 
    
    Console.println(longest)
    if( longest.isDefined ){
      Console.println(getPath(longest.get))
      Console.println(getPath(longest.get).size)
    }

  }
  
  // val code = "hijkl"
  // val code = "ihgpwlah"
  // val code = "kglvqrro"
  // val code = "ulqzkmiv"
  val code = "mmsxrhfx"
  
  val solutions = ListBuffer[Step]()
  var longest : Option[Step] = None
  
  def solved( step : Step ) = {
    solutions += step
    
    longest match {
      case Some(s) => {
        if( s.steps.size <= step.steps.size ){
          longest = Some(step)
        }
      }
      case None => {
        longest = Some(step)
      }
    }
  }
  
  def tooLong( step : Step ) : Boolean = {
    
    longest match {
      case Some(s) => {
        s.steps.size <= step.steps.size
      }
      case None => { false }
    }
  }
  
  def solve( start : Step ) = {
    
    val work = Queue[Step]()
    
    work.enqueue(start)
    
    while( !work.isEmpty ){
      
      // start at the current spot
      val current = work.dequeue()
      
      // is this a solution?
      if( isSolution( current ) ){
        solved( current )
        // else if( tooLong( current ) ) {
        // already have a shorter path
        // }
      }
      else {

        // get the path so far
        val path = getPath( current ) 
      
        // calc the hash
        val hash = calcHash( code + path )
      
        // calc the open/closed
        val moves = calcOpen( hash )
        
        if( !moves.isEmpty ){
      
          // follow the opens
          for( mv <- moves ){
          
            // create the next step 
            val next = nextStep( current, mv )
          
            // is the step valid? 
            if( isValid( next ) ){
            
              // queue it
              work.enqueue(next)
            }
            else {
              // not valid
            }

          }
        }
        else {
          // dead end
        }

      }
      
    }
    
  }
  
  def isValid( step : Step ) : Boolean = {
    
    if( step.x < 0 || step.x > 3 ){
      false
    }
    else if( step.y < 0 || step.y > 3 ){
      false
    }
    else {
      true
    }
    
  }
  
  def nextStep( step : Step, dir : Dir ) : Step = { 
    new Step( step.x + dir.x, step.y + dir.y, dir, step.steps :+ step )
  }
  
  def getPath( step : Step ) : String = {
    
    var p = ""
    
    // start with the list
    for( s <- step.steps ){
      s.dir match {
        case s : Start => {}
        case d : Dir => { p = p + d.code }
      }
    }
    
    // then tack on the current
    step.dir match {
      case s : Start => {}
      case d : Dir => { p = p + d.code }
    }
    
    p
    
  }
  
  def isSolution( step : Step ) : Boolean = {
    step.x == 3 && step.y == 3
  }
  
  class Dir( val x : Int, val y : Int, val code : Char )
  case class Start() extends Dir( 0, 0, 'S' )
  case class Up() extends Dir( 0, -1, 'U' )
  case class Down() extends Dir( 0, 1, 'D' )
  case class Right() extends Dir( 1, 0, 'R' )
  case class Left() extends Dir( -1, 0, 'L' )
  
  class Step( val x : Int, val y : Int, val dir : Dir, val steps : List[Step] )
  
  val OPEN = List( 'b', 'c', 'd', 'e', 'f' )
  
  def calcOpen( str : String ) : List[Dir] = {
    val open = ListBuffer[Dir]()
    
    // up, down, left, right
    if( OPEN.contains( str(0) ) ){
      open += Up()
    }
    
    if( OPEN.contains( str(1) ) ){
      open += Down()
    }
    
    if( OPEN.contains( str(2) ) ){
      open += Left()
    }
    
    if( OPEN.contains( str(3) ) ){
      open += Right()
    }

    open.toList
  }
  

  def calcHash( input : String ) : String = {
    val bytes = MessageDigest.getInstance("MD5").digest(input.getBytes)
    val md5hash1 = bytes.map("%02x".format(_)).mkString
    md5hash1
  }
}