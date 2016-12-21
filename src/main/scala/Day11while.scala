

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

object Day11while {

  // val elements = List( 'H', 'L' )
  val elements = List( 'P', 'C', 'U', 'R', 'L' )

  class Building( var floors : List[List[Int]], var steps : List[Building] )

  def main( args : Array[String] ) : Unit = {

    Console.println( "day11..." )

    val f1 = List( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 )
    val f2 = List( 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0 )
    val f3 = List( 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 )
    val f4 = List( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )

    /*
    val f1 = List( 1, 0, 1, 0, 1 )
    val f2 = List( 0, 1, 0, 0, 0 )
    val f3 = List( 0, 0, 0, 1, 0 )
    val f4 = List( 0, 0, 0, 0, 0 )
    * 
    */

    val start = new Building( List( f1, f2, f3, f4 ), List() )

    print( start )

    solve( start )

    Console.println( "done..." + solutions.size )
    printSteps( shortestSolution.get )
    Console.println( shortestSolution.get.steps.size )

  }

  var shortest = Int.MaxValue
  var shortestSolution : Option[Building] = None

  val solutions = ListBuffer[Building]()
  def addSolution( bld : Building ) = {
    solutions += bld
    // calc shortest
    calcShortest()
  }

  def calcShortest() = {
    for ( s <- solutions ) {
      if ( s.steps.size < shortest ) {
        shortest = s.steps.size
        shortestSolution = Some( s )
        Console.println( shortest )
      }
    }
  }

  def printSteps( bld : Building ) = {
    for ( s <- bld.steps ) {
      print( s )
    }
    print( bld )
  }
  
  val visited = ListBuffer[Building]()
  def hasVisited( b : Building ) : Boolean = {
    
    val idx = visited.indexWhere( equals( _, b ) )
    if( idx > -1 ){
      if( visited(idx).steps.size <= b.steps.size ){
        true
      }
      else {
        visited += b
        false
      }
    }
    else {
      visited += b
      false
    }
  }

  def solve( start : Building ) : Unit = {

    Console.println( "solve..." )

    var work = Queue[Building]()

    work.enqueue( start )

    while ( !work.isEmpty ) {

      val current = work.dequeue()
      
      if( !isBuildingSafe( current ) ){
        Console.println( "not safe..." )
      }
      else if( shortestSolution.isDefined && shortestSolution.get.steps.size < current.steps.size ){
        Console.println( "stop..." )
      }
      else if( isDone( current ) ){
        Console.println( "solved..." )
        addSolution(current)
      }
      else if( isLoop( current ) ){
        Console.println( "loop..." )
      }
      else if( hasVisited( current ) ){
        Console.println( "been there..." )
      }
      else {  
      
      Console.println( "solving..." )
      Console.println( work.size )
      Console.println( solutions.size )
      Console.println( shortest )
      Console.println( current.steps.size )

      //  where is the elevator
      val floor = currentFloor( current )
      val floorNo = current.floors.indexWhere( _( 0 ) == 1 )

      // generate the possible moves from current floor
      val moves = generateMoves( floor )

      if ( moves.size < 1 ) {
        Console.println( "dead end" )
      } else {

        for ( mv <- moves ) {

          // apply each move up
          if ( floorNo < current.floors.size - 1 ) {
            val m = move( current, floorNo, floorNo + 1, mv ) 
            if( isBuildingSafe(m) ){
              work.enqueue( m )
            }
          }

          // apply each move down
          if( floorNo > 0 ) {
            val m = move( current, floorNo, floorNo - 1, mv ) 
            if( isBuildingSafe(m) ){
              work.enqueue( m )
            }
          }
        }
        // for

        }
      }
    }
    // while
  }
  
  def isLoop( bld : Building ) : Boolean = {
    // is the current state anywhere in the steps to this state?
    bld.steps.exists( (b:Building) => { equals( bld, b ) } ) 
  }

  def equals( b1 : Building, b2 : Building ) : Boolean = {

    var ok = true

    for ( i <- 0 until b1.floors.size ) {
      for ( j <- 0 until b1.floors( i ).size ) {
        if ( b1.floors( i )( j ) != b2.floors( i )( j ) ) {
          ok = false
        }
      }
    }

    ok
  }

  def move( start : Building, from : Int, to : Int, mv : List[Int] ) : Building = {

    val floors = ListBuffer() ++ start.floors
    floors( from ) = sub( floors( from ), mv )
    floors( to ) = add( floors( to ), mv )

    new Building( floors.toList, ( start.steps :+ start ) )
  }

  def sub( floor : List[Int], move : List[Int] ) : List[Int] = {
    val ret = for ( i <- 0 until floor.size ) yield { floor( i ) - move( i ) }
    ret.toList
  }

  def add( floor : List[Int], move : List[Int] ) : List[Int] = {
    val ret = for ( i <- 0 until floor.size ) yield { floor( i ) + move( i ) }
    ret.toList
  }

  def currentFloor( bld : Building ) : List[Int] = {
    bld.floors.filter( _( 0 ) == 1 ).head
  }

  def floorEmpty( floor : List[Int] ) : Boolean = {
    floor.forall( _ == 0 )
  }

  def generateMoves( start : List[Int] ) : List[List[Int]] = {

    val items = start.tail

    val all = for ( i <- 0 until ( BigInt( 2 ) ).pow( items.size ).toInt ) yield {
      val s = i.toBinaryString.reverse
      val p = s.padTo( elements.size * 2, '0' )
      p.reverse
    }

    val validCount = all.filter( ( s : String ) => {
      val ones = s.filter( ( c : Char ) => { c == '1' } )
      ones.size <= 2 && ones.size > 0
    } )

    val validPos = validCount.filter( ( s : String ) => {

      var ok = true

      for ( p <- 0 until s.size ) {

        val c1 = s( p ).toInt - 48
        val c2 = items( p )
        if ( c1 == 1 && c2 == 0 ) {
          ok = false
        }
      }

      ok

    } )

    val l = validPos.map( ( s : String ) => {
      val t = s.map( ( c : Char ) => { c.toInt - 48 } )
      t.toList
    } )

    val ret = l.map( 1 +: _ )

    ret.toList

  }

  def isDone( bld : Building ) : Boolean = {
    bld.floors.last.forall( _ == 1 )
  }

  def isBuildingSafe( bld : Building ) : Boolean = {
    bld.floors.forall( isFloorSafe( _ ) )
  }

  def isFloorSafe( ps : List[Int] ) : Boolean = {

    // if the floor is empty its safe
    if ( ps.forall( _ == 0 ) ) {
      true
    } else {

      val parts = ps.tail

      val rtgs = for ( i <- 0 until parts.size by 2 if parts( i ) > 0 ) yield parts( i )

      for ( i <- 1 until parts.size by 2 ) {
        if ( parts( i ) == 1 && parts( i - 1 ) != 1 ) {
          // unprotected - are there other rtgs?
          if ( rtgs.size > 0 ) {
            return false
          }
        }
      }

      true
    }

  }

  def print( bld : Building ) = {

    Console.println( " " )

    for ( i <- bld.floors.size until 0 by -1 ) {

      Console.print( "F" + i )
      Console.print( " " )
      if ( bld.floors( i - 1 )( 0 ) == 1 ) {
        Console.print( "E" )
      } else {
        Console.print( "." )
      }
      Console.print( "  " )

      for ( pos <- 2 to bld.floors( i - 1 ).size by 2 ) {

        val c = elements( ( pos / 2 ) - 1 )

        if ( bld.floors( i - 1 )( pos - 1 ) == 1 ) {
          Console.print( c + "G" )
        } else {
          Console.print( "." )
        }
        Console.print( "  " )

        if ( bld.floors( i - 1 )( pos ) == 1 ) {
          Console.print( c + "M" )
        } else {
          Console.print( "." )
        }
        Console.print( "  " )
      }

      Console.print( '\n' )

    }

    Console.println( " " )

  }

}