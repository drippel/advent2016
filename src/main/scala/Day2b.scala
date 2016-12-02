

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils

object Day2b {
  
  case class Location( x : Int, y : Int )
  
  class Direction( val x : Int, val y : Int )
  case class Up() extends Direction( 0, 1 )
  case class Left() extends Direction( -1, 0 )
  case class Down() extends Direction( 0, -1 )
  case class Right() extends Direction( 1, 0 )
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day2...")
    
    val directions = read( "./src/day2.txt" )

    
    var currentLocation = Location(-2,0)
    
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
    
    val code = locationToCode( location )
    val loc = code match {
      case '1' => {
        step match {
          case Down() => { add( location, step ) }
          case _ => { location }
        }
      }
      case '5' => { 
        step match {
          case Right() => { add( location, step ) }
          case _ => { location }
        }
      }
      case '9' => {
        step match {
          case Left() => { add( location, step ) }
          case _ => { location }
        }
      }
      case 'D' => {
        step match {
          case Up() => { add( location, step ) }
          case _ => { location }
        }
        
      }
      case _ => {
        bound( add( location, step ) ) 
      }
    }
    
    loc
  }
  
  def bound( location : Location ) : Location = {
    
    if( java.lang.Math.abs( location.x ) < 2 && java.lang.Math.abs( location.y ) < 2 ){
      location
    }
    else if( java.lang.Math.abs( location.x ) == 2 && location.y == 0 )  {
      location
    }
    else if( java.lang.Math.abs( location.y ) == 2 && location.x == 0 )  {
      location
    }
    else {
      Location( bound(location.x), bound(location.y) )
    }
    
  }
  
  def bound( i : Int ) : Int = {
    if( i < -1 ) { -1 }
    else if( i > 1 ) { 1 }
    else { i }
  }
  
  def add( location : Location, step : Direction ) : Location = {
    Location( location.x + step.x, location.y + step.y )
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
  
  def locationToCode( location : Location ) : Char = {
    
    location match {
      case Location(-1,1) => '2'
      case Location(0,1) => '3'
      case Location(1,1) => '4'
      case Location(-1,0) => '6'
      case Location(0,0) => '7'
      case Location(1,0) => '8'
      case Location(-1,-1) => 'A'
      case Location(0,-1) => 'B'
      case Location(1,-1) => 'C'
      case Location(-2,0) => '5'
      case Location(0,2) => '1'
      case Location(2,0) => '9'
      case Location(0,-2) => 'D'
      case _ => throw new IllegalStateException( "invalid location" )
    }
    
  }
  
  
  
}