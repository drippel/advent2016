

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

object Day13 {
  
  case class Point( x : Int, y : Int )
  
  case class Path( points : List[Point] ) 
  
  val X = 50
  val Y = 60
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day13...")
    
    val grid = Array.ofDim[Char](X,Y)
    
    for( y <- 0 to ( Y - 1 ) ){
      for( x <- 0 to ( X - 1 ) ) {
        // grid(x)(y) = toChar(x,y,10)
        grid(x)(y) = toChar(x,y,1352)
      }
      Console.print( '\n' )
    }

    Console.print( '\n' )
    
    Console.println( isOpen( 2, 1, 10 ) )
    print(grid)
    
    // val solutions = walk( Point(1,1), Point(7,4), grid )
    val solutions = walk( Point(1,1), Point(31,39), grid )
    
    if( solutions.isEmpty ){
      Console.println("nope")
    }
    else {
      solutions.foreach( print( grid, _ ) )
      solutions.foreach( (p:Path) => { Console.println( p.points.size - 1 ) } )
    }
    
  }
  
  def print( grid : Array[Array[Char]], path : Path ) = {
    
    for( y <- 0 to ( Y - 1 ) ){
      for( x <- 0 to ( X - 1 )  ){
        if( path.points.contains(Point(x,y)) ){
          Console.print( '0' )
        }
        else {
          Console.print( grid(x)(y) )
        }
      }
      Console.print( '\n' )
    }
    Console.print( '\n' )
  }

  def walk( start : Point, target : Point, grid : Array[Array[Char]] ) : List[Path] = {
    val solutions = ListBuffer[Path]()
    
    val initial = Path( List(start) )
    
    val work = Queue[Path]()
    
    work.enqueue(initial)
    
    while( !work.isEmpty ){
      
      val current = work.dequeue()
      
      if( !solutions.isEmpty && current.points.length >= solutions.last.points.length  ){
        // too long
        Console.println("too long")
      }
      else if( current.points.last == target ){
        Console.println("solution")
        solutions += current 
      }
      else {
        
        // generate, validate, prevent loopback
        var moves = generate( current.points.last )
        moves = moves.filter( valid( grid, _ ) ) 
        moves = moves.filter( !loopback( current.points, _ ) )
        
        if( !moves.isEmpty ){
          for( mv <- moves ){
            val next = Path( current.points :+ mv )
            work.enqueue(next)
          }
        }
        else {
          // Console.println("dead end")
        }
        
        
      }
      
    }
    
    solutions.toList
  }
  
  def loopback( points : List[Point], point : Point ) = {
    points.contains(point)
  }
  
  def valid( grid : Array[Array[Char]], point : Point ) : Boolean = {
    
    if( point.x < 0 || point.y < 0 ){
      false
    }
    else if( point.x >= X || point.y >= Y ) {
      false
    }
    else{
      grid(point.x)(point.y) match {
        case '#' => { false }
        case '.' => { true }
        case _ => { throw new IllegalStateException( "whaa" ) }
      }
    }
    
  }
  
  def generate( start : Point ) : List[Point] = {
    
    var moves = ListBuffer[Point]()
    
    moves += Point( start.x + 0, start.y + 1 )
    moves += Point( start.x + 1, start.y + 0 )
    moves += Point( start.x + 0, start.y + -1 )
    moves += Point( start.x + -1, start.y + 0 )
    
    moves.toList
    
  }
  
  def print( grid : Array[Array[Char]] ) = {
    
    for( y <- 0 to ( Y - 1 ) ){
      for( x <- 0 to ( X - 1 )  ){
        if( x == 31 && y == 39 ){
          Console.print( '0' )
        }
        else {
          Console.print( grid(x)(y) )
        }
      }
      Console.print( '\n' )
    }
    Console.print( '\n' )
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def toChar( x : Int, y : Int, fav : Int ) : Char = {
    if( isOpen( x, y, fav ) ) { '.' }
    else { '#' }
  }
  
  def isOpen( x : Int, y : Int, fav : Int ) : Boolean = {
    // val i = ( x * x ) + ( 3 * x ) + ( 2 * x * y ) + y + ( x * y ) + fav 
    val j = x * x + 3 * x + 2 * x * y + y + y * y + fav 
    // Console.println( i +" "+ j )
    val s = j.toBinaryString
    // Console.println( s )
    val count = s.filter( _ == '1' )
    count.size % 2 == 0
  }

}