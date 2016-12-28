

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day21 {
  
  class Instruction()
  case class SwapPos( x : Int, y : Int ) extends Instruction
  case class SwapLetter( a : Char, b : Char ) extends Instruction
  case class Rotate( dir : Char, x : Int ) extends Instruction
  case class RotatePos( c : Char ) extends Instruction
  case class Reverse( x : Int, y : Int ) extends Instruction
  case class Move( x : Int, y : Int ) extends Instruction
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day21...")
    
    val lines = read( "./src/day21.txt" )
    
    //val steps = lines.map( parse( _ ) )
    
    // steps.foreach( Console.println( _ ) )
    
    // val input = "abcde"
    
    // val output = steps.foldLeft(input)( apply )
    
    var input = "abcdefgh"
    Console.println(input)
    var steps = List(RotatePos('a'))
    var output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))
    
    input = "habcdefg"
    Console.println(input)
    steps = List(RotatePos('a'))
    output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))

    input = "ghabcdef"
    Console.println(input)
    steps = List(RotatePos('a'))
    output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))

    input = "fghabcde"
    Console.println(input)
    steps = List(RotatePos('a'))
    output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))

    input = "efghabcd"
    Console.println(input)
    steps = List(RotatePos('a'))
    output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))

    input = "defghabc"
    Console.println(input)
    steps = List(RotatePos('a'))
    output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))

    input = "cdefghab"
    Console.println(input)
    steps = List(RotatePos('a'))
    output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))

    input = "bcdefgha"
    Console.println(input)
    steps = List(RotatePos('a'))
    output = steps.foldLeft(input)( apply )
    Console.println(output)
    Console.println(input.indexOf('a'))
    Console.println(output.indexOf('a'))
  }
  
  def apply( input : String, step : Instruction ) : String = {
    
    // Console.println(step)
    // Console.println(input)

    var working = ListBuffer[Char]() ++ input.toList
    
    step match {
      case SwapPos(x,y) => {
        val a = working(x)
        val b = working(y)
        working(x) = b
        working(y) = a
      }
      case SwapLetter(a,b) => {
        val p1 = working.indexOf(a)
        val p2 = working.indexOf(b)
        working(p2) = a
        working(p1) = b
      }
      case Rotate(dir,x) => {
        if( dir == 'r' ){
          working = working.reverse
        }
        
        for( i <- 0 until x ){
          working = working.tail :+ working.head
        }

        if( dir == 'r' ){
          working = working.reverse
        }
      }
      case RotatePos(c) => {
        
        var p = working.indexOf(c) 
        if( p >= 4 ){
          p = p + 2
        }
        else {
          p = p + 1
        }
        working = working.reverse

        for( i <- 0 until p ){
          working = working.tail :+ working.head
        }

        working = working.reverse
        
      }
      case Reverse(x,y) => {
        
        val start = working.slice(0, x)
        val end = working.slice(y+1,working.size)
        
        val mid = working.slice(x, y+1)
        
        working = start ++ mid.reverse ++ end
        
      }
      case Move(x,y) => {
        val c = working.remove(x)
        working.insert(y,c)
      }
    }
    
    
    // Console.println(working.mkString)
    working.mkString
  }

  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def parse( line : String ) : Instruction = {
    val parts = line.split(" ")
    parts(0) match {
      case "swap" => {
        parts(1) match {
          case "position" => { SwapPos( parts(2).toInt, parts(5).toInt )}
          case "letter" => { SwapLetter( parts(2)(0), parts(5)(0) ) }
          case _ => { throw new IllegalStateException("invalid step") }
        }
      }
      case "reverse" => { Reverse( parts(2).toInt, parts(4).toInt ) }
      case "rotate" => { 
        parts(1) match {
          case "left" => { Rotate( 'l', parts(2).toInt ) }
          case "right" => { Rotate( 'r', parts(2).toInt ) }
          case "based" => { RotatePos( parts(6)(0) ) }
          case _ => { throw new IllegalStateException("invalid step") }
        }
      }
      case "move" => { Move( parts(2).toInt, parts(5).toInt ) }
      case _ => { throw new IllegalStateException("unknown instruction") }
    }
  }
  
}