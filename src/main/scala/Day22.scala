

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

object Day22 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day22...")
    
    var lines = read( "./src/day22.txt" )
    
    // discard the first two
    lines = lines.tail
    lines = lines.tail
    
    val nodes = lines.map( parse( _ ) )
    
    Console.println( nodes.size )
    
    val ps = makePairs(nodes)
    
    Console.println(ps.size)
    
    val g = makeArray(nodes)
    printArray(g)

    val c = copy(g)
    printArray(c)
    
    // nodes.filter( _.avail > 50 ).foreach( Console.println( _ ) )
    
    val s1 = Path( 0, 0, List() ) 
    val s2 = Path( 1, 0, ( s1.steps :+ s1) ) 
    val s3 = Path( 2, 0, ( s2.steps :+ s2) ) 
    
    Console.println( isBacktrack( s3, s1 ) )
    
    printPath(s3)
    
    solve(g)
    
  }
  
  def printPath( path : Path ) = {
    
    Console.print( "P:" + path.steps.size +" " )
    
    for( p <- path.steps ){
      Console.print( "("+ p.x +", "+ p.y +") " )
    }
    Console.print( "(" +  path.x +", "+ path.y +")" )
    Console.print( "\n" )

  }

  def copy( grid : Array[Array[Node]] ) : Array[Array[Node]] = {

    val cp = Array.ofDim[Node](39,25)
    
    for( y <- 0 until 25 ){
      for( x <- 0 until 39 ){
        cp(x)(y) = grid(x)(y) 
      }
    }
    
    cp
  }
  
  def findFreespace( grid : Array[Array[Node]] ) : Path = {
    
    var p : Option[Path] = None
    
    
    for( y <- 0 until 25 ){
      for( x <- 0 until 39 ){
        if( grid(x)(y).used == 0 ){
          p = Some( Path( x, y, List() ) ) 
        }
      }
    }
    
    p match {
      case Some(path) => { path }
      case None => { throw new IllegalStateException("empty not found") } 
    }
    
  }
  
  def solve( grid : Array[Array[Node]] ) : Unit = {
    
    var moves = 0
    
    var result = copy(grid)
    
    // step 1 
    // find the shortest possible route based on disksizes only
    // this is the best path for the data to follow
    val goalPath = findShortestGoal(  Path( 38, 0, List()), Path(0,0,List()), List(), grid )
    printPath( goalPath )
    
    val goalSteps = goalPath.steps.tail :+ Path( goalPath.x, goalPath.y, List() )
    
    var currentLocation = goalPath.steps.head
    
    // step 2 
    for( coords <- goalSteps ){
    
      Console.println( coords.x, coords.y )
      

      val free = findFreespace(result)
      val freePath = findShortestGoal( free, coords, List(currentLocation), result )
      // printPath( freePath )
    
      // move the freespace to the coord
      moves = moves + freePath.steps.size
      result = moveFreespace( freePath, result )

      // move the current goal location to the freespace
      moves = moves + 1
      swap( currentLocation.x, currentLocation.y, freePath.x, freePath.y, result )
      printArray(result)

      currentLocation = coords

    }
    
    Console.println( moves )

    
  }
  
  def moveFreespace( path : Path, grid : Array[Array[Node]] ) : Array[Array[Node]] = {
    
    var cp = copy(grid)
    
    Console.println( "From:"+ path.steps.head +" To:"+ path.x +","+ path.y )
    
    for( i <- 0 until path.steps.size - 1 ){
      
      val from = path.steps(i)
      val to = path.steps(i+1)
      
      swap( from.x, from.y, to.x, to.y, cp )
      
    }
    
    // and then from last to the coords of path
    swap( path.steps.last.x, path.steps.last.y, path.x, path.y, cp )
    
    cp

  }
  
  def swap( fromx : Int, fromy : Int, tox : Int, toy : Int, grid : Array[Array[Node]] ) = {
    
      val oldFrom = grid(fromx)(fromy)
      val oldTo = grid(tox)(toy)
      
      // calc the new from node
      // case class Node( x : Int, y : Int, size : Int, used : Int, avail : Int, use : Int ) 
      val newFrom = Node( fromx, fromy, oldFrom.size, oldTo.used, oldFrom.size - oldTo.used, (oldTo.used / oldFrom.size ) )
      val newTo = Node( tox, toy, oldTo.size, oldFrom.used, oldTo.size - oldFrom.used, (oldFrom.used / oldTo.size ) )
      
      grid(tox)(toy) = newTo
      grid(fromx)(fromy) = newFrom
  }
  
  case class Path( val x : Int, val y : Int, steps : List[Path] )
  
  class Direction( val x : Int, val y : Int )
  case class North() extends Direction( 0, -1 )
  case class East() extends Direction( 1, 0 )
  case class South() extends Direction( 0, 1 )
  case class West() extends Direction( -1, 0 )
  
  val dirs = List(North(),South(),East(),West())
  
  def distance( p1 : Path, p2 : Path ) : Double = {
    distance( p1.x, p2.x, p1.y, p2.y )
  }

  def distance( x1 : Int, x2 : Int, y1 : Int, y2 : Int ) = {
    Math.sqrt( Math.pow( (x1 - x2), 2) + Math.pow((y1 - y2), 2))
  }

  class ByDistance( goal : Path ) extends Ordering[Path] {
      def compare( p1 : Path, p2 : Path ) : Int = {
        val d1 = distance( goal, p1 )
        val d2 = distance( goal, p2 )
        d1.compareTo(d2)
      }
    }
  
  var visited = ListBuffer[Path]()
  
  def isVisited( p : Path ) : Boolean = {
    val path = visited.find( (p2:Path) => { (p2.x == p.x) && (p2.y == p.y) } )
    path match {
      case Some(p2) => {
        if( p2.steps.size <= p.steps.size ){
          // we have a shorter route already
          true
        }
        else {
          // this is a new shorter route
          visited -= p2 
          visited += p
          false
        }
      }
      case None => {
        // new route
        visited += p 
        false
      }
    }
  }
  
  def isImmovable( path : Path, immovable : List[Path] ) : Boolean = {
    immovable.exists( (p:Path) => { p.x == path.x && p.y == path.y } )
  }
  
  def findShortestGoal( start : Path, goal : Path, immovable : List[Path], grid : Array[Array[Node]] ) : Path = {

    visited = ListBuffer[Path]()
    
    
    // based on disk sizes find the shortest route possible
    val work = PriorityQueue[Path]()( new ByDistance(goal).reverse )
    
    var shortest : Option[Path] = None
    
    visited += start

    work.enqueue(start)
    
    while( !work.isEmpty ){

      val current = work.dequeue()
      
      if( work.size % 100 == 0 ){
        Console.println(work.size)
      }
      
      // is this a solution
      if( current.x == goal.x && current.y == goal.y ){
        // solved
        if( shortest.isDefined ){
          if( shortest.get.steps.size > current.steps.size ){
            // we have a new solution
            shortest = Some(current)
          }
          else {
            // already have a solution that is shorter 
          }
        }
        else {
          Console.println("solution")
          shortest = Some(current)
        }
      }
      else {
        // not a solution
        
        // is our current path longer than the solution?
        if( shortest.isDefined && shortest.get.steps.size < current.steps.size ){
          // longer than the current solution - done
          // Console.println("longer")
        }
        else {

          // lets try some moves
          val allmoves = dirs.map( generate( current, _ ) )
          val validmoves = allmoves.filter( isValid(_) )
          val nobacks = validmoves.filter( !isBacktrack( current, _ ) )
          val hassize = nobacks.filter( (p:Path) => { grid(current.x)(current.y).used <= grid(p.x)(p.y).size })
          val notvisited = hassize.filter( !isVisited(_) )
          val notimmovable = notvisited.filter( !isImmovable( _, immovable ) )
          
          for( m <- notimmovable ){
            work.enqueue(m)
          }

        }
      }
    }
    
    shortest.get
    
  }
  
  def isValid( path : Path ) : Boolean = {
    
    if( path.x < 0 || path.y < 0 ){
      false
    }
    else if( path.x > 38 || path.y > 24 ){
      false
    }
    else {
      true
    }
  }
  
  def isSame( p1 : Path, p2 : Path ) : Boolean = {
    p1.x == p2.x && p1.y == p2.y
  }
  
  def isBacktrack( current : Path, next : Path ) : Boolean = {
    current.steps.exists( isSame( _, next ) )
  }
  
  def generate( current : Path, dir : Direction ) : Path = {
    Path( current.x + dir.x, current.y + dir.y, (current.steps :+ current) )
  }
  
  def printArray( grid : Array[Array[Node]] ) = {
    
    Console.println("-" )
    Console.print( "    " )
    for( i <- 0 until 39 ){
      val n = i.toString().reverse.padTo(8, " " ).reverse.mkString
      Console.print( n )
    }
    Console.print( "\n" )
    
    for( y <- 0 until 25 ){
      val p = y.toString().reverse.padTo(4, " " ).reverse.mkString
      Console.print(p)
      Console.print(" ")
      for( x <- 0 until 39 ){
        
        val s = grid(x)(y).used +"/" + grid(x)(y).size
        Console.print(s.reverse.padTo(7, " " ).reverse.mkString)
        Console.print(" ")
        
      }
      Console.print( "\n" )
    }

    Console.print( "\n" )
    Console.println("-" )
  }
  
  def makeArray( nodes : List[Node] ) : Array[Array[Node]] = {
    
    val grid = Array.ofDim[Node](39,25)
    
    for( n <- nodes ){
      grid(n.x)(n.y) = n
    }
    
    grid
  }
  
  def makePairs( nodes : List[Node] ) : List[(Node,Node)] = {
    
    val pairs = ListBuffer[(Node,Node)]()
    
    for( i <- 0 until nodes.size ){
      for( j <- 0 until nodes.size ) {
        
        if( i != j ){
          if( nodes(i).used != 0 ){
            if( nodes(i).used <= nodes(j).avail ){
              val p = (nodes(i),nodes(j))
              pairs += p
            }
          }
        }
      }
    }
    
    pairs.toList
    
  }
  
  
  
  case class Node( x : Int, y : Int, size : Int, used : Int, avail : Int, use : Int ) 

  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def parse( line : String ) : Node = {
    
    Console.println(line)
    
    val parts = line.split(" ").filter( _.trim().length() > 0 )
    
    val dparts = parts(0).split("-")
    
    val x = dparts(1).tail.toInt
    val y = dparts(2).tail.toInt
    
    Node( x, y, parts(1).replace("T", "" ).toInt, 
        parts(2).replace("T", "" ).toInt, 
        parts(3).replace("T", "" ).toInt,
        parts(4).replace("%", "" ).toInt )
    
  }

  
}