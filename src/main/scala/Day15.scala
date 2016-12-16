

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day15 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day15...")
    
    val lines = read( "./src/day15b.txt" )
    
    lines.foreach( Console.println(_) ) 
    
    val discs = lines.map( convert( _ ) )
    
    val lock = Lock( discs )
    
    tick(lock)
    
    
  }
  
  def tick( start : Lock ) = {

    
    var l = start
    var t = 0
    
    var done = false 
    
    // for( i <- 0 to 10 ){
    while( !done ) {

      l = spin(l)

      val s = drop( l, Ball(t) )
      l = s._1

      print( t, l)
      
      // check for ball
      s._2 match {
        case Some(b:Ball) => {
          done = true
          Console.println( "winnah winnah :" + b.time )
        }
        case _ => { }
      }
      
      t = t + 1
      
    }
  }
  
  def toLB( lock : Lock ) : ListBuffer[ListBuffer[Position]] = {
    val lb = ListBuffer[ListBuffer[Position]]()
    lb ++= lock.discs.map( toLB( _ ) )
    lb
  }
  
  def toLB( disc : Disc ) :  ListBuffer[Position] = {
    var lb = ListBuffer[Position]()
    lb ++= disc.pos
    lb
  }
  
  def drop( lock : Lock, ball : Ball ) : (Lock,Option[Ball]) = {
    
    var ret : Option[Ball] = None
    
    // fuck up here - we need to actually set values across the discs
    val lbs = toLB( lock )
    
    for( i <- lbs.size - 1 to 0 by -1 ) {
      for( j <- 0 until lbs(i).size ) {
        lbs(i)(j) match {
          case Open() => { Open() }
          case Closed() => { Closed() }
          case b : Ball => { 
            // drop it to the next level
            if( ( i + 1 ) == lock.discs.size ){
              // or if this is the last level its the stopper
              Console.println("through") 
              ret = Some(b)
              lbs(i)(j) = Open()
            }
            else {
              // try dropping to next level
              // if the next level is open at 0 drop it
              lbs(i+1)(0) match {
                case Open() => {
                  // drop the ball down to the level 
                  Console.println("drop") 
                  lbs(i+1)(0) = b
                  lbs(i)(j) = Open()
                }
                case _ => { 
                  Console.println("plonk") 
                  lbs(i)(j) = Open()
                }
              }
            }
          }
        }
      }
    }
    
    // drop the incoming ball
    lbs(0)(0) match {
      case Open() => { lbs(0)(0) = ball }
      case _ => {
        Console.println("plonk")
      }
    }
    
    val ds = for( l <- lbs ) yield {
      Disc( l.toList )
    }
    
    (Lock( ds.toList ),ret)
    
  }
  
  def isBall( p : Position ) : Boolean = {
    p match {
      case b : Ball => { true }
      case Open() => { false }
      case Closed() => { false }
    }
  }
  
  def spin( lock : Lock ) : Lock = {
    Lock( lock.discs.map( spin( _ ) ) )
  }
  
  def spin( disc : Disc ) : Disc = {
    
    val r = disc.pos.reverse
    val n = r.tail :+ r.head
    Disc( n.reverse )

  }
  
  def isOpen( disc : Disc ) : Boolean = {
    
    disc.pos.head match {
      case Open() => { true }
      case _ => { false }
    }
    
  }
  
  def print( time : Int, lock : Lock ) : Unit = {
    Console.println( "t:" + time )
    print(lock)
  }
  
  def print( lock : Lock ) = {
    
    Console.println("--")
    for( d <- 0 until lock.discs.size ){
      
      Console.print( d + " " )
      
      for( i <- 0 until lock.discs(d).pos.size ){
        
        lock.discs(d).pos(i) match {
          case Open() => { Console.print( "V  " ) } 
          case Closed() => { Console.print( "-  " ) }
          case Ball(t) => { Console.print( "O  " ) }
        }
        
      }
      
      Console.print('\n')
    }
    Console.println("--")

  }
  
  
  
  case class Lock( var discs : List[Disc] ) 
  case class Disc( val pos : List[Position]  )

  class Position( val c : Char )
  case class Open() extends Position( 'V' )
  case class Closed() extends Position( '-' )
  case class Ball( val time : Int ) extends Position( 'O' )
  
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def convert( line : String ) : Disc = {
    val parts = line.split( " " )
    
    val s = parts(3).toInt
    val o = parts(11).replace('.', ' ' ).trim().toInt
    
    val ps = ListBuffer[Position]()
    for( i <- 0 until s ){
      if( i == o ){
        ps += Open()
      }
      else {
        ps += Closed() 
      }
    }
    
    Disc( ps.toList )
    
    
  }

}