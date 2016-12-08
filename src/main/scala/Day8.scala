

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day8 {
  
  class Instruction()
  case class Rect( x : Int, y : Int ) extends Instruction()
  case class Rotate( dir : Int, pos : Int, amount : Int ) extends Instruction() 
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day8...")
    
    val lines = read( "./src/day8.txt" )
    val instructions = lines.map( convert( _ ) )
    
    val end = instructions.foldLeft(initialGrid())(apply)

    printGrid(end)
    
    Console.println( lit( end ) )
  }
  
  def lit( grid : Array[Array[Char]] ) : Int = {
    
    var count = 0
    
    val f = grid.flatten
    
    for( c <- f ){
      if( c == '#' ){
        count = count + 1
      }
    }
    
    count
    
  }
  
  def apply( start : Array[Array[Char]], instruction : Instruction ) : Array[Array[Char]] = {
    
    // copy the starting array?
    // Console.println( instruction )
    
    instruction match {
      case rect : Rect => {
        
        for( y <- 0 until rect.y ){
          for( x <- 0 until rect.x ){
            start(y)(x) = '#'
          }
        }

      }
      case rotate : Rotate => {

        rotate.dir match {
          case 0 => {
            // rotate a row
            val line = shift( start(rotate.pos), rotate.amount )
            start(rotate.pos) = line
          }
          case 1 => {
            // rotate a col
            val col = shift( get( start, rotate.pos ), rotate.amount )
            set( start, rotate.pos, col )
          }
          case _ => { throw new IllegalArgumentException("bad dir") }
        }

      }
      case _ => { throw new IllegalArgumentException( "bad instruction" ) }
    }
    

    // printGrid(start)
    start
  }
  
  def get( grid : Array[Array[Char]], pos : Int ) : Array[Char] = {
    val col = Array.ofDim[Char](6)
    
    for( p <- 0 until 6 ){
      col(p) = grid(p)(pos)
    }
    
    /*
    printGrid(grid)
    Console.print( pos + " - " )
    for( c <- col ){
      Console.print( c )
    }
    Console.print( '\n' )
    * 
    */
    
    col
  }
  
  def set( grid : Array[Array[Char]], pos : Int, col : Array[Char] ) = {
    
    for( p <- 0 until 6 ){
      grid(p)(pos) = col(p)
    }
    
  }
  
  def shift( line : Array[Char], amount : Int ) : Array[Char] = {

    
    for( i <- 0 until amount ){
      
      // get the last char
      val c = line(line.length-1) 
      for( p <- (line.length - 1) until 0 by -1 ){
        line(p) = line(p-1)
      }
      line(0) = c
      
    }
    
    line
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def initialGrid() = {
    
    val grid = Array.ofDim[Char](6,50)
    
    for( y <- 0 until 6 ){
      for( x <- 0 until 50 ){
        grid(y)(x) = '.'
      }
    }
    
    grid
  }
  
  def printGrid( grid : Array[Array[Char]] ) = {
    
    for( y <- 0 until 6 ){
      for( x <- 0 until 50 ){
        Console.print( grid(y)(x) )
      }
      Console.print( '\n' )
    }

    Console.print( '\n' )
    Console.print( '\n' )
  }
  
  def convert( line : String ) : Instruction = {
    
    val parts = line.split(' ')
    
    parts(0) match {
      case "rect" => {
        val is = parts(1).split('x')
        new Rect( is(0).toInt, is(1).toInt )
      }
      case "rotate" => {

        val dir = parts(1) match {
          case "column" => { 1 }
          case "row" => { 0 }
          case _ => { throw new IllegalStateException( "hmm" ) }
        }
        
        val pos = parts(2).split('=')

        new Rotate( dir, pos(1).toInt, parts(4).toInt )
      }
      case _ => {
        throw new IllegalStateException( "nope" )
      }
    }

  }
  
}