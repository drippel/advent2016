

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

object Day18 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day18...")
    
    val start = ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^"
    // val start = ".^^.^.^^^^"
    // val start = "^^^...^..^"
    // Console.println(start)
    // Console.println(next(start))
    
    // val rows = generate( start, 400000 )
    /*
    val rows = generate( start, 40 )
    rows.foreach( Console.println(_) )
    
    val f = rows.flatten
    
    val c = f.foldLeft(0)( (i:Int,c:Char) => { 
      if( c == '.' ) {
        i + 1
      }
      else {
        i 
      }
    })
    * 
    */
    
    // Console.println(c)
    Console.println(count(start,400000))

  }

  def count( start : String, size : Int ) : Int = {
    
    var count = 0
    
    count += start.filter( _ == '.' ).size
    
    var current = start
    
    for( i <- 1 until size ){

      current = next( current )
      count += current.filter( _ == '.' ).size
    }
    
    count 
    
  }
  
  def generate( start : String, size : Int ) : List[String] = {
    
    val rows = ListBuffer[String]()
    
    rows += start
    
    for( i <- 1 until size ){
      rows += next( rows(i-1) )
    }
    
    rows.toList
    
  }
  
  def next( start : String ) : String = {
    
    var n = ""
    
    for( i <- 0 until start.size ){
      
      val l = if( ( i - 1 ) > -1 ){ start(i -1) }else{'.'}
      val c = start(i)
      val r = if( (i + 1) > (start.size - 1) ){ '.' }else{ start(i+1) }
      
      val t = if( l == '^' && c == '^' && r == '.' ){
        '^'
      }
      else if( l == '.' && c == '^' && r == '^' ){
        '^'
      }
      else if( l == '^' && c == '.' && r == '.' ) {
        '^' 
      }
      else if( l == '.' && c == '.' && r == '^' ) {
        '^'
      }
      else { '.' }
      
      n = n + t
      
    }
    
    
    n
    
  }

}