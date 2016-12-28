

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day20a {
  
  case class SRange( val lo : Long, val hi : Long ){
    def in( l : Long ) : Boolean =  {
      lo <= l && l <= hi
    }
  }
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day20...")
    
    val lines = read( "./src/day20.txt" )
    

    val ps = lines.map( parse( _ ) )
    
    val lowest = ps.sortWith(lower)
    // val highest = ps.sortWith(higher)
    
    lowest.foreach( Console.println( _ ) )
    
    // Console.println( lowest.head )
    // Console.println( highest.head )
    
    // val start = SRange( 0L, 4793564 )
    // val start = SRange( 0L, 10L )
    // Console.println( start )
    
    // val s2 = SRange( 0L, 2L )
    
    // val rs = diff( start, s2 )
    var count = 0L
    var idx = 0L 
    while( idx <= 4294967295L ){
      // Console.println(i)
      val sr = lowest.find( (r:SRange) => { r.in(idx) } )
      if( !sr.isDefined ){
         count = count + 1
        Console.println( idx +", "+ count)
      }
      idx = idx + 1
    }
    
    Console.println(count)
    
  }
  
  def contains( s1 : SRange, s2 : SRange ) : Boolean = {
    
    true
    
  }
  
  def diff( s1 : SRange, s2 : SRange ) : List[SRange] = {
    
    var ranges = ListBuffer[SRange]()
    
    // cases
    
    if( s2.hi < s1.lo ){
      // 1. s2 is all less than s1 s2.hi < s1.lo
      ranges += s1
    }
    else if( s2.lo < s1.lo && s1.in( s2.hi ) ){
      // 2. overlap 1 - s2.lo < s1.lo && s1.lo < s2.hi < s1.hi
    }
    else if( s1.in( s2.lo ) && s2.hi > s1.hi ) {
      // 3. overlap 2 - s2.hi > s1.hi && s1.lo < s2.lo < s1.hi
    }
    else { 
    // 4. s1 contains all of s2 - s1.lo < s2.lo && s1.hi > s2.hi 
    // 5. s2 contains all of s1 - s2.lo < s1.lo && s2.hi > s1.hi 
    // 6. s2 is all greater than s1 s2.lo > s1.hi
    }
    
    ranges.toList
    
  }
  
  
  
  def lower( p1 : SRange, p2 : SRange ) : Boolean = {
    if( p1.lo < p2.lo ){
      true
    }
    else {
      p1.hi < p2.hi 
    }
  }

  def higher( p1 : SRange, p2 : SRange ) : Boolean = {
    if( p1.hi > p2.hi ){
      true
    }
    else {
      p1.lo > p2.lo 
    }
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def parse( line : String ) : SRange = {

    val parts = line.split("-")
    SRange(parts(0).toLong,parts(1).toLong)
  }
  
}