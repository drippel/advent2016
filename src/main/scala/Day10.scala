

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day10 {
  
  class Chip( val value : Int )
  case class ValueChip( i : Int ) extends Chip(i)
  case class Empty() extends Chip(0)

  
  class Bot( val name : Int, var lo : Chip, var hi : Chip, var locon : Either[Int,Bot], var hicon : Either[Int,Bot] ){
    
    override def toString() : String = {
      var s = "[" + name +","+ lo + "," + hi + ","

      
      val l =locon match {
        case Left(i) => { "O:" + i }
        case Right(b) => { "B:"+ b.name }
      }
      
      s = s + l
      
      val h = hicon match {
        case Left(i) => { "O:" + i }
        case Right(b) => { "B:"+ b.name }
      }
      
      s = s +","+ h
      
      s = s + "]"
      
      s
    }
    
  }
  
  val bots = HashMap[Int,Bot]()
  val outputs = HashMap[Int,Chip]()
  
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day10...")
    
    val lines = read( "./src/day10.txt" )
    
    createBots(lines)
    connectBots(lines)
    processChips(lines)
    Console.println(bots)
    Console.println(outputs)
    
    
    // 
    val found = bots.find( (kv : (Int,Bot)) => { 
      if( kv._2.hi.value == 61 ){
        if( kv._2.lo.value == 17 ){
          true
        }
        else {
          false
        }
      }
      else {
        false
      }

    })
    
    found match {
      case Some(b) => { Console.println(b) }
      case _ => { Console.println("doh") }
    }
    
    Console.println( outputs(0).value * outputs(1).value * outputs(2).value )
    
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def createBots( lines : List[String] ) = {
    
    for( line <- lines ){
      val parts = StringUtils.split(line)
      if( parts(0).equalsIgnoreCase("bot") ){
        
        val idx = parts(1).toInt
        val bot = new Bot( idx, Empty(), Empty(), Left(-1), Left(-1) )
        
        bots += ( idx -> bot )
        
      }
    }
    
  }
  
  def connectBots( lines : List[String] ) = {
    
    for( line <- lines ){
      
      val parts = StringUtils.split(line)
      
      if( parts(0).equalsIgnoreCase("bot") ){
        
        bots.get(parts(1).toInt ) match {
          case Some(bot) => { 
            
            bot.locon = parts(5) match {
              case "output" => { Left(parts(6).toInt) }
              case "bot" => { Right( bots.get(parts(6).toInt).get )}
            }
            
            bot.hicon = parts(10) match {
              case "output" => { Left(parts(11).toInt) }
              case "bot" => { Right( bots.get(parts(11).toInt).get )}
            }
          }
          case _ => { throw new IllegalStateException("no bot") }
        }
        
      }
      
    }
    
  }
  
  def giveChip( bot : Bot, chip : Chip ) : Unit = {
    
    chip match {
      case v : ValueChip => {
        
        // does the bot have a chip?
        if( bot.lo == Empty() && bot.hi == Empty() ){
          bot.lo = v
        }
        else {
          
          // determine which is hi and which is low
          if( bot.lo.value < v.value ){
            bot.hi = v
          }
          else {
            bot.hi = bot.lo
            bot.lo = v
          }
          
          // now we have two chips -- hand out the chips
          
          bot.locon match {
            case Left(i) => { outputs += ( i -> bot.lo  ) }
            case Right(b) => { giveChip( b, bot.lo ) }
          }
          
          bot.hicon match {
            case Left(i) => { outputs += ( i -> bot.hi ) }
            case Right(b) => { giveChip( b, bot.hi ) }
          }
        }
      }
      case _ => { throw new IllegalArgumentException("bad chip") }
    }
    
  }
  
  def processChips( lines : List[String] ) : Unit = {
    
    for( line <- lines ){
      val parts = StringUtils.split(line)
      if( parts(0).equalsIgnoreCase("value") ){
        
        val c = ValueChip(parts(1).toInt)
        
        val b = bots.get(parts(5).toInt)
        
        b match {
          case Some(bot) => { giveChip(bot,c) }
          case _ => { throw new IllegalStateException("invalid bot") }
        }
        
      }
    }
    
  }
  
}