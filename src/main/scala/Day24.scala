

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.PriorityQueue

object Day24 {
  
  def main( args : Array[String] ) : Unit = {

    Console.println("day24...")
    
    val lines = read( "./src/day24.txt" )

    val X = lines.head.size
    val Y = lines.size
    
    val maze = convert( lines )
    
    print(maze,X,Y)
    
    val points = findPoints( maze, X, Y )
    
    val (start,goals) = points.partition( _.c == '0' )
    
    Console.println("start:"+ start )
    // Console.println("goals:"+ goals )
    
    val perms = goals.permutations.toList
    
    // Console.println( shortestPath( start.head, perms.head.head, maze ) ) 

    var lo = Int.MaxValue
    for( i <- 0 until perms.size ){
      Console.println(i)
      print(perms(i))
      val steps = solve( start.head, perms(i), start.head, maze, lo )
      Console.println( steps )
      if( steps < lo ){
        lo = steps
      }
      Console.println(lo)
    }
    Console.println(lo)
    
    
  }
  
  def solve( start : Point, goals : List[Point], end : Point, maze : Array[Array[Char]], currLow : Int ) : Int = {
    
    var current = start
    
    // print(goals)
    var total = 0
    
    for( g <- goals ){
      val steps = solve( current, g, maze )
      total = total + steps
      current = g
    }
    
    // then go back to the start
    val back = shortestPath( goals.last, end, maze )
    total = total + back.size - 1
    
    return total
  }
  
  def solve( start : Point, goal : Point, maze : Array[Array[Char]] ) : Int = {
    // Console.println( "start: ("+ start.x +","+ start.y +") to:("+ goal.x +","+ goal.y +")" )
    val p = shortestPath( start, goal, maze )
    p.size - 1
  }

  def print( path : List[Point] ) = {
    
    Console.print( "P:" )
    for( p <- path ){
      Console.print( "(" + p.x +"," + p.y +"," + p.c +") " )
    }
    Console.print( "\n" )
    
  }
  
  def distance( p1 : Point, p2 : Point ) : Double = {
    distance( p1.x, p2.x, p1.y, p2.y )
  }

  def distance( x1 : Int, x2 : Int, y1 : Int, y2 : Int ) = {
    Math.sqrt( Math.pow( (x1 - x2), 2) + Math.pow((y1 - y2), 2))
  }

  class ByDistance( goal : Point ) extends Ordering[List[Point]] {
      def compare( p1 : List[Point], p2 : List[Point] ) : Int = {
        val d1 = distance( goal, p1.head )
        val d2 = distance( goal, p2.head )
        d1.compareTo(d2)
      }
  }

  var visited = ListBuffer[List[Point]]() 
  
  def hasVisited( points : List[Point] ) : Boolean = {
    
    val path = visited.find( (l:List[Point]) => { l.head.x == points.head.x && l.head.y == points.head.y } )
    
    path match {
      case Some(p) => {
        if( p.size <= points.size ){
          true
        }
        else {
          visited -= p
          visited += points 
          false
        }
      }
      case None => {
        visited += points 
        false
      }
    }
    
  }

  def shortestPath( start : Point, goal : Point, maze : Array[Array[Char]] ) : List[Point] = {
    
    var shortest : Option[List[Point]] = None
    visited = ListBuffer[List[Point]]()
    
    val work = PriorityQueue[List[Point]]()( (new ByDistance(goal)).reverse )
    work.enqueue(List(start))
    
    while( !work.isEmpty ){
      
      /*
      if( work.size % 1000 == 0 ){
        Console.println(work.size)
      }
      */
      
      val path = work.dequeue()
      val current = path.head 
      
      if( current.x == goal.x && current.y == goal.y ) {
        // Console.println("solution...")
        if( shortest.isDefined ){
          if( shortest.get.size > path.size ){
            shortest = Some(path)
          }
        }
        else {
          shortest = Some(path)
        }
      }
      else if( shortest.isDefined && shortest.get.size <= path.size ) {
        // bail
      }
      else {
        
        val all = generate( path.head )
        val valid = all.filter( isValid( _, maze ) )
        val nobacktrack = valid.filter( !isBacktrack( _, path ) )
        
        // queue up work
        for( p <- nobacktrack ){
          val n = p +: path
          if( !hasVisited(n) ){
            work.enqueue(n)
          }
        }
        
      }
      
    }
    
    shortest.get
    
  }
  
  def isBacktrack( point : Point, path : List[Point] ) : Boolean = {
    path.exists( (p:Point) => { p.x == point.x && p.y == point.y } )
  }
  
  def isValid( point : Point, maze : Array[Array[Char]] ) : Boolean = {
    
    val x = maze.size
    val y = maze(0).size
    
    if( point.x < 0 || point.y < 0 ){
      false
    }
    else if( point.x >= x || point.y >= y ) {
      false
    }
    else {
      maze(point.x)(point.y) != '#'
    }
    
  }
  
  def generate( point : Point ) : List[Point] = {
    dirs.map( (d:Move) => { Point( point.x + d.x, point.y + d.y, '.' ) } ) 
  }
  
  
  class Move( val x : Int, val y : Int )
  case class Up() extends Move( 0, -1 )
  case class Right() extends Move( 1, 0 )
  case class Down() extends Move( 0, 1 )
  case class Left() extends Move( -1, 0 )
  val dirs = List(Up(), Right(), Down(), Left() )
  
  def findPoints( grid : Array[Array[Char]], mx : Int, my : Int ) : List[Point] = {
    
    val points = ListBuffer[Point]()
    
    for( y <- 0 until my ){
      for( x <- 0 until mx ){
        if( grid(x)(y) >= '0' && grid(x)(y) <= '9' ){
          points += Point( x, y, grid(x)(y) )
        }
      }
    }
    
    points.toList
    
  }
  
  case class Point( x : Int, y : Int, c : Char )
  
  def print( grid : Array[Array[Char]], x : Int, y : Int ) = {
    for( j <- 0 until y ){
      for( i <- 0 until x ){
        Console.print( grid(i)(j) )
      }
      Console.print( '\n' )
    }
    
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def convert( lines : List[String] ) : Array[Array[Char]] = {
    
    val X = lines.head.size
    val Y = lines.size
    
    val grid = Array.ofDim[Char](X,Y)
    
    for( j <- 0 until Y ){
      for( i <- 0 until X ){
        grid(i)(j) = lines(j)(i)
      }
    }
    
    grid
    
  }
  
  
}