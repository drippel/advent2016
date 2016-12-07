

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day7 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day7...")
    
    val lines = read( "./src/day7.txt" )
    val ips = lines.map( hypernets( _ ) )
    Console.println(ips)
    val tls = ips.filter( hasTLS( _ ) )
    // Console.println(tls.size)
    
    val ssls = ips.filter( hasSSL( _ ) )
    Console.println(ssls.size)
  }
  
  def hasSSL( parts : (List[String],List[String]) ) : Boolean = {
    
    Console.println( parts._1 + "|" + parts._2 )
    
    // find all the ABAs in the supernets 
    val babs = parts._1.map( aba( _ ) ).flatten 
    Console.println( babs )
    
    // does the aba exist in any of the hypernets
    parts._2.exists( hasBAB( _ , babs ) ) 
    
  }
  
  def hasBAB( net : String, babs : List[String] ) : Boolean = {
    babs.exists( net.contains( _ ) )
  }
  
  def aba( net : String ) : List[String] = {
    
    val abas = ListBuffer[String]()
    
    for( pos <- 0 to (net.size - 3) ){
      
      val c1 = net(pos)
      val c2 = net(pos+1)
      val c3 = net(pos+2)
      
      if( c1 == c3 && c1 != c2 ){
        val aba = "" + c2 + c1 + c2 
        abas += aba
      }
      
    }
    
    
    abas.toList
    
  }
  
  def hasTLS( parts : (List[String],List[String]) ) : Boolean = {
    hasAbba( parts._1 ) && !hasAbba( parts._2 )
  }
  
  def hasAbba( nets : List[String] ) : Boolean = {
    nets.exists( isAbba(_) ) 
  }
  
  def isAbba( net : String ) : Boolean = {
    
    for( pos <- 0 to (net.size - 4) ){
      
      val c1 = net(pos)
      val c2 = net(pos+1)
      val c3 = net(pos+2)
      val c4 = net(pos+3)
      
      if( c1 == c4 ){
        if( c2 == c3 ){
          if( c1 != c2 ){
            return true
          }
        }
      }
      
    }
    
    false
  }

  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def hypernets( line : String ) : (List[String],List[String]) = {
    
    val ips = ListBuffer[String]()
    val hs = ListBuffer[String]()
    
    var inHypernet = false
    var current = "" 
    
    for( pos <- 0 until line.length() ){
      
      if( line(pos) == '[' ){
        
        ips += current
        current = ""
        
      }
      else if( line(pos) == ']' ){
        hs += current
        current = ""
      }
      else {
        current += line(pos)
      }
      
    }
    
    ips += current
    
    (ips.toList,hs.toList)
    

    
  }
  
}