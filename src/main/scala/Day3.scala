

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils

object Day3 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day3...")
    
    val combos = read( "./src/day3.txt" )
    
    
    val valid = combos.filter( isValid( _) )
    Console.println( valid.size )
    
    val col1 = combos.map( _._1 )
    val col2 = combos.map( _._2 )
    val col3 = combos.map( _._3 )
    
    val combined = col1 ++ col2 ++ col3
    
    val ts = toTuples( combined )
    
    val valid2 = ts.filter( isValid( _ ) )
    Console.println( valid2.size )
  }
  
  def toTuples( list : List[Int] ) : List[(Int,Int,Int)] = {
    
    val ts = for( i <- 0 until list.size by 3 ) yield {
      (list(i),  list( i + 1 ), list( i + 2 ))
    }
    
    ts.toList
  }
  
  def isValid( t : (Int,Int,Int) ) : Boolean ={
    isValid( t._1, t._2, t._3 )
  }
  
  def isValid( a : Int, b : Int, c : Int ) : Boolean = {
    
    if( a + b <= c ){
      false
    }
    else if( a + c <= b ){
      false
    }
    else if ( b + c <= a ) {
      false
    }
    else {
      true 
    }
    
  }
  
  def read( file : String ) : List[(Int,Int,Int)] = {
    
    val lines = IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    
    val c = lines.map( convert(_) )_
    
    c.toList

  }
  
  def convert( line : String ) : (Int,Int,Int) = {
    
    val parts = StringUtils.split(line.trim())
    
    (parts(0).toInt,parts(1).toInt,parts(2).toInt)
    
  }
  
}