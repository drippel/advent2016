

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils

object Day1 {
  
  class Direction( val x : Int, val y : Int )
  case class North() extends Direction( 0, 1 )
  case class East() extends Direction( 1, 0 )
  case class South() extends Direction( 0, -1 )
  case class West() extends Direction( -1, 0 )
  
  case class Location( val x : Int, val y : Int, val dir : Direction )

  
  class Turn()
  case class Right() extends Turn()
  case class Left() extends Turn()
  
  case class Step( val turn : Turn, val len : Int )
  
  val visited = scala.collection.mutable.HashMap[(Int,Int),Int]()
  val doubles = ListBuffer[(Int,Int)]()
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day1...")
    
    // load the steps
    val steps = readSteps("src/day1-steps.txt")
    
    // initialize the position
    val start = Location( 0, 0, North() )
    visit(0,0)
    
    val end = steps.foldLeft( start )( applyStep )
    
    Console.println(end)
    Console.println(doubles.head)
  }
  
  def visit( x : Int, y : Int ) = {
    val t = (x,y)
    
    val c = visited.get(t) match {
      case Some(count) => {
        if( count == 1 ){
          doubles += t 
        }
        count + 1 
      }
      case None => {1}
    }
    
    visited.put(t, c)

  }
  
  def applyStep( loc : Location, step : Step ) : Location = {
    
    // turn first
    val newDir = turn( loc.dir, step.turn )
    
    var x = loc.x
    var y = loc.y
    
    for( i <- 1 to step.len ){ 
      x = x + ( newDir.x * 1 )
      y = y + ( newDir.y * 1 )
      visit( x, y )
    }
    
    // val x = loc.x + ( newDir.x * step.len )
    // val y = loc.y + ( newDir.y * step.len )
    Location( x, y, newDir )
    
  }
  
  def turn( currentDir : Direction, currentTurn : Turn ) : Direction = {
    
    currentTurn match {
      case Right() => {
        
        currentDir match {
          case North() => { East() }
          case East() => { South() }
          case South() => { West() }
          case West() => { North() }
        }
        
      }
      case Left() => {
        
        currentDir match {
          case North() => { West() }
          case East() => { North() }
          case South() => { East() }
          case West() => { South() }
        }

      }
    }
    
  }
  
  
  def readSteps( file : String ) : List[Step] = {
    
    val steps = ListBuffer[Step]();
    
    val lines = IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    
    for( line <- lines ){
      
      val ss = StringUtils.split(line, ',')
      val ts = ss.map( (s:String) => { toStep( s.trim() ) } ).toList 
      steps ++= ts

    }
    
    steps.toList
    
  }
  
  def toStep( s : String ) : Step = {
    
    val d = s.substring(0,1) match {
      case "R" => Right()
      case "L" => Left()
      case _ => throw new IllegalStateException()
    }
    val l = s.substring(1)
    
    Step( d, l.toInt )
    
  }
  
}