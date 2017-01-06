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
import java.util.Date
import scala.collection.mutable.PriorityQueue

object Day11Part2 {

  case class State(val ele: Int, pairs: List[Pair], states: List[State]) extends Ordered[State] {

    def compare(that: State): Int = {
      if (ele == that.ele) {
        val z = pairs.zip(that.pairs)

        val pt = z.find((t: (Pair, Pair)) => { !t._1.equals(t._2) })
        pt match {
          case s: Some[(Pair, Pair)] => {
            s.get._1.compare(s.get._2)
          }
          case None => { 0 }
        }
      } else {
        ele.compare(that.ele)
      }
    }
    
    def isFloorClear( floor : Int ) : Boolean = {
      pairs.forall( (p:Pair) => { p.chip != floor && p.rtg != floor } )
    }
    
    def isClearBelow() : Boolean = {
      pairs.forall( (p:Pair) => { p.chip > ele && p.rtg > ele } )
    }

    def floorToString(floor: Int): String = {

      var f = ""

      if (floor == ele) {
        f = f + "1"
      } else {
        f = f + "0"
      }

      // loop through the pairs
      for (p <- pairs) {

        if (floor == p.rtg) {
          f = f + "1"
        } else {
          f = f + "0"
        }

        if (floor == p.chip) {
          f = f + "1"
        } else {
          f = f + "0"
        }
      }

      f

    }

    def stateToString(): String = {
      // build a string
      var s = ""

      for (f <- 0 until 4) {
        s = s + floorToString(f)
      }

      s
    }

    def toLong(): Long = {
      java.lang.Long.parseLong(stateToString(), 2)
    }

    def isSolved() : Boolean = {
      pairs.forall((p: Pair) => { p.chip == 3 && p.rtg == 3 })
    }

    def isLoop(): Boolean = {
      states.contains(this)
    }

    def isSafe(): Boolean = {

    val unprotected = for (p <- pairs if p.chip != p.rtg) yield p

    // for each unprotected chip - is there a gen on that floor
    if (unprotected.size > 0) {

      !unprotected.exists((p1: Pair) => {
        pairs.exists((p2: Pair) => { p2.rtg == p1.chip })
      })

    } else {
      true
    }
  }
  }

  case class Pair(rtg: Int = 0, chip: Int = 0) extends Ordered[Pair] {
    def compare(that: Pair): Int = {
      if (rtg == that.rtg) {
        chip.compare(that.chip)
      } else {
        rtg.compare(that.rtg)
      }

    }
  }

  def main(args: Array[String]): Unit = {

    Console.println("day11...")

    /*
     *                G  M  G  M  G  M  G  M  G  M
    val f4 = List( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
    val f3 = List( 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 )
    val f2 = List( 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0 )
    val f1 = List( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 )
    */

    /*
    val f4 = List( 0, 0, 0, 0, 0 )
    val f3 = List( 0, 0, 0, 1, 0 )
    val f2 = List( 0, 1, 0, 0, 0 )
    val f1 = List( 1, 0, 1, 0, 1 )
    */

    val pairs = ListBuffer[Pair]()
    // 1
    // pairs += Pair(1, 0)
    // pairs += Pair(2, 0)

    // j
    // pairs += Pair( 0, 0 )
    // pairs += Pair( 1, 2 )
    // pairs += Pair( 1, 2 )
    // pairs += Pair( 1, 2 )
    // pairs += Pair( 1, 2 )

    // 2
     pairs += Pair( 0, 0 )
     pairs += Pair( 1, 2 )
     pairs += Pair( 1, 2 )
     pairs += Pair( 1, 2 )
     pairs += Pair( 1, 2 )
     pairs += Pair( 0, 0 )
     pairs += Pair( 0, 0 )

    val start = State(0, pairs.sorted.reverse.toList, List())
    print(start)

    Console.println(start.isSafe())

    // val ele = elevatorFloor( start )
    // Console.println( ele )
    // Console.println( generateMoves( List( 1, 1, 1, 0 ), start.pairs.size ) )

    solve(start)

    if (shortest.isDefined) {
      print(shortest.get)
      Console.println(shortest.get.states.size)
      printSolution(shortest.get)
      Console.println(shortest.get.states.size)
    }
    else {
      Console.println("hmmm...")
    }
    

  }

  def printSolution(state: State) = {

    for (s <- state.states) {
      print(visited.get(s.toLong()).get)
    }

    print(state)

  }

  var shortest: Option[State] = None

  def addSolution(state: State) : Unit = {
      if (shortest.isDefined) {
        if (state.states.size < shortest.get.states.size) {
          shortest = Some(state)
        }
      } else {
        shortest = Some(state)
      }
  }

  var visited = HashMap[Long, State]()

  def isVisited(state: State): Boolean = {
    val l = state.toLong()
    visited.get(l) match {
      case Some(s) => {

        if (state.states.size < s.states.size) {
          visited.put(l, state)
          false
        } else {
          // the current path is shorter
          true
        }

      }
      case None => {
        visited.put(l, state)
        false
      }
    }
  }

  def isTooLong(state: State): Boolean = {
    if (shortest.isDefined) {
      state.states.size >= shortest.get.states.size
    } else {
      false
    }
  }

  val ds = List(1, 3, 7, 11)

  def distance(state: State): Int = {

    // do we care where the elevator is?
    // var sum = ds(state.ele)
    var sum = 0 

    for (p <- state.pairs) {
      sum += ds(p.chip)
      sum += ds(p.rtg)
    }

    sum

  }

  class ByDistance() extends Ordering[State] {
    def compare(s1: State, s2: State): Int = {
      val d1 = distance(s1)
      val d2 = distance(s2)
      d1.compare(d2)
    }
  }

  def solve(start: State) = {

    var checked = 0L
    var lo = 0
    var hi = 0
    var tm = (new Date()).getTime

    // var work = Queue[State]()
    var work = PriorityQueue[State]()((new ByDistance()).reverse)

    isVisited(start)

    work.enqueue(start)

    while (!work.isEmpty) {

      checked = checked + 1

      if (lo < work.head.states.size && work.head.states.size == work.last.states.size) {
        // Console.println( "sort queue..." )
        // val dnew = work.sortWith(greater)
        // work = dnew
        lo = work.head.states.size
      }

      val current = work.dequeue()

      if (work.size > 1000 && work.size % 10000 == 0) {
        // Console.println( "lo,hi:"+ work.head.states.size +","+ work.last.states.size )
        Console.println("w:" + work.size)
        Console.println("c:" + checked)
        Console.println("v:" + visited.size)
        Console.println("t:" + ((new Date()).getTime() - tm))
        tm = (new Date()).getTime
        print( work.head )
        print( work.last )
      }

      if( current.isSolved() ) {
        Console.println("solved...")
        addSolution(current)
        print(current)
      } else {

        val ele = elevatorFloor(current)
        val mvs = generateMoves(ele, current.pairs.size)
        // val mvs = generateMoves( current )

        // for each move
        for (mv <- mvs) {
          up(current, mv) match {
            case Some(up) => {
              if (isVisited(up)) {
                checked = checked + 1
              } else if (up.isLoop()) {
                checked = checked + 1
              } else if (isTooLong(up)) {
                checked = checked + 1
              } else if (!up.isSafe()) {
                checked = checked + 1
              } else {
                work.enqueue(up)
              }
            }
            case None => {
              checked = checked + 1
            }
          }
          
          if( !current.isClearBelow() ){

          down(current, mv) match {
            case Some(down) => {
              if (isVisited(down)) {
                checked = checked + 1
              } else if (down.isLoop()) {
                checked = checked + 1
              } else if (isTooLong(down)) {
                checked = checked + 1
              } else if (!down.isSafe()) {
                checked = checked + 1
              } else {
                work.enqueue(down)
              }
            }
            case None => {
              checked = checked + 1
            }
          }
          }
        }
      }

    }

  }

  def up(state: State, is: List[Int]): Option[State] = {
    if (state.ele >= 3) {
      None
    } else {
      Some(apply(state, state.ele + 1, is))
    }
  }

  def down(state: State, is: List[Int]): Option[State] = {
    if (state.ele == 0) {
      None
    } else {
      val l = is.map(_ * -1)
      Some(apply(state, state.ele - 1, l))
    }
  }

  def apply(state: State, ele: Int, is: List[Int]): State = {

    // 
    val pairs = ListBuffer[Pair]()

    for (i <- 0 until state.pairs.size) {

      val rtg = if (state.ele == state.pairs(i).rtg) {
        state.pairs(i).rtg + is(i * 2)
      } else {
        state.pairs(i).rtg
      }

      val chip = if (state.ele == state.pairs(i).chip) {
        state.pairs(i).chip + is((i * 2) + 1)
      } else {
        state.pairs(i).chip
      }

      pairs += Pair(rtg, chip)
    }

    // create a new state 
    State(ele, pairs.sorted.toList, state.states :+ state )
  }

  def elevatorFloor(state: State): List[Int] = {
    val ps = ListBuffer[Int]()

    for (p <- state.pairs) {
      if (state.ele == p.rtg) {
        ps += 1
      } else {
        ps += 0
      }
      if (state.ele == p.chip) {
        ps += 1
      } else {
        ps += 0
      }
    }

    ps.toList
  }


  def print(state: State) = {
    Console.println("-" + state.isSolved() )
    Console.println(state.states.size)
    for (i <- 3 to 0 by -1) {

      if (state.ele == i) {
        Console.print("E ")
      } else {
        Console.print(". ")
      }

      for (p <- state.pairs) {

        if (p.rtg == i) {
          Console.print("G ")
        } else {
          Console.print(". ")
        }

        if (p.chip == i) {
          Console.print("C ")
        } else {
          Console.print(". ")
        }
      }

      Console.print('\n')

    }
    Console.println("-")
  }

  def generateMoves(floor: List[Int], noOfPairs: Int): List[List[Int]] = {

    val all = for (i <- 0 until (BigInt(2)).pow(noOfPairs * 2).toInt) yield {
      val s = i.toBinaryString.reverse
      val p = s.padTo(noOfPairs * 2, '0')
      p.reverse
    }

    val validCount = all.filter((s: String) => {
      val ones = s.filter((c: Char) => { c == '1' })
      ones.size <= 2 && ones.size > 0
    })

    val l = validCount.map((s: String) => {
      val t = s.map((c: Char) => { c.toInt - 48 })
      t.toList
    })

    // screen by floor
    val validForFloor = l.filter((is: List[Int]) => {
      var ok = true
      for (i <- 0 until is.size) {
        if (is(i) == 1 && floor(i) == 0) {
          ok = false
        }
      }
      ok
    })

    validForFloor.toList

  }

}