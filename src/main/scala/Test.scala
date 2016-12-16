

object Test {
  
  def main( args : Array[String] ) : Unit = {
    
    val a1 = "0000000000".toCharArray()
    
    val a2 = "1111111111".toCharArray()
    
    a1.copyToArray( a2 )
    
    print( a2 )
    
    
  }
  
  def print( chars : Array[Char] ) = {
    for( i <- 0 until chars.length ){
      Console.print( chars(i) )
    }
    Console.print("\n")
  }
  
  
  def generateMoves( start : List[Int]) : List[List[Int]] = {
    
    val items = start.tail
    
    val all = for( i <- 0 until (BigInt(2)).pow(items.size).toInt ) yield {
      val s = i.toBinaryString.reverse 
      val p = s.padTo(items.size, '0') 
      p.reverse
    }
    
    val validCount = all.filter( (s:String) => {
      val ones = s.filter( (c:Char) => { c == '1' } )
      ones.size <= 2 && ones.size > 0
    })  
    
    val validPos = validCount.filter( (s:String) => {
      
      var ok = true
      
      for( p <- 0 until s.size ){
        
        val c1 = s(p).toInt - 48
        val c2 = items(p) 
        if( c1 == 1 && c2 == 0 ){
          ok = false
        }
      }
      
      ok

    })
    
    val l = validPos.map( (s:String) => {
      val t = s.map( (c:Char) => { c.toInt - 48 })
      t.toList
    })
    
    val ret = l.map( 1 +: _ ) 
    
    ret.toList
    
    
    
  }
}