

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils

object Day2 {
  
  case class Location( x : Int, y : Int )
  
  class Direction( val x : Int, val y : Int )
  case class Up() extends Direction( 0, 1 )
  case class Left() extends Direction( -1, 0 )
  case class Down() extends Direction( 0, -1 )
  case class Right() extends Direction( 1, 0 )
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day2...")
    
    val directions = read( "./src/day2.txt" )

    
    var currentLocation = Location(0,0)
    
    val ends = for( line <- directions ) yield {
      currentLocation = walk( currentLocation, line )
      currentLocation
    }
    
    val code = ends.map( locationToCode( _ ) ) 
    
    Console.println( code )
    
    
  }
  
  def walk( location : Location, steps : List[Direction] ) : Location = {
    
    var currentLocation = Location( location.x, location.y )
    
    for( step <- steps ){
      
      currentLocation = move( currentLocation, step )
      
    }
    
    currentLocation
    
  }
  
  def move( location : Location, step : Direction ) : Location = {
    var x = bound( location.x + step.x ) 
    var y = bound( location.y + step.y ) 
    Location( x, y )
  }
  
  def bound( i : Int ) : Int = {
    if( i > 1 ){ 
      1 
    }
    else if( i < -1 ){ 
      -1 
    }
    else {
      i
    }
  }
  
  def read( file : String ) : List[List[Direction]] = {
    
    val lines = IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    
    val ts = for( line <- lines ) yield { convert( line ) }
    ts.toList 
  }
  
  def convert( line : String ) : List[Direction] = {
    
    val ds = for( c <- line ) yield {
      
      c match {
        case 'U' => Up()
        case 'D' => Down()
        case 'R' => Right()
        case 'L' => Left()
        case _ => throw new IllegalStateException("unknown direction")
      }
      
    }
    
    ds.toList 
    
  }
  
  def locationToCode( location : Location ) : Int = {
    
    location match {
      case Location(-1,1) => 1
      case Location(0,1) => 2
      case Location(1,1) => 3
      case Location(-1,0) => 4
      case Location(0,0) => 5
      case Location(1,0) => 6
      case Location(-1,-1) => 7
      case Location(0,-1) => 8
      case Location(1,-1) => 9
      case _ => throw new IllegalStateException( "invalid location" )
    }
    
  }
  
  
  
}