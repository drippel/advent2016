

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

import akka.actor.Actor
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Props
import akka.event.Logging
import akka.routing.RoundRobinPool
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import akka.pattern.ask
import scala.util.Failure
import scala.util.Success
import scala.concurrent.ExecutionContext
import scala.concurrent.Await

object Day11Actors {

  case class Pair(rtg: Int, chip: Int)
  case class State(val ele: Int, pairs: List[Pair], states: List[Long]){
    def isLoop() : Boolean = {
      states.contains(stateToLong(this))
    }

    def isSafe() : Boolean = {

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

  def floorToString(floor: Int, state: State): String = {

    var f = ""

    if (floor == state.ele) {
      f = f + "1"
    } else {
      f = f + "0"
    }

    // loop through the pairs
    for (p <- state.pairs) {

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

  def stateToString(state: State): String = {
    // build a string

    var s = ""

    for (f <- 0 until 4) {
      s = s + floorToString(f, state)
    }

    s
  }

  def stateToLong(state: State): Long = {
    java.lang.Long.parseLong(stateToString(state), 2)
  }

  def main(args: Array[String]): Unit = {

    Console.println("day11...")

    // actor stuff
    try {

      val config = ConfigFactory.load()
      val system = ActorSystem.create("akka", config)
      val controller = system.actorOf(Props(new Controller()), "controller")

      controller ! "start"

      try {
        Thread.sleep(10000)
      } catch {
        case t: Throwable => {}
      }

      //ocontroller ! "end"

    } catch {
      case t: Throwable => {
        t.printStackTrace()
      }
    }
  }

  class Controller extends Actor {

    val starter = context.actorOf(Props(new Starter()), "starter")
    val solvers = context.actorOf(RoundRobinPool(20).props(Props(new Solver())), "solver")
    val collector = context.actorOf(Props(new Collector()), "collector")
    val visitor = context.actorOf(Props(new Visitor()), "visitor")

    def receive = {

      case "start" => {
        Console.println("starting...")
        context.actorSelection("/user/controller/starter") ! "start"
        context.become(wrapup)

      }

    }

    def wrapup: Receive = {

      case "end" => {
        Console.println("stopping...")
        context.system.terminate()
      }

    }

  }

  class Starter extends Actor {
    def receive = {
      case s: String => {
        Console.println("starting...")
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

        val start = State(0, pairs.sortWith(compare).toList, List())

        context.actorSelection("/user/controller/solver") ! start

      }
    }
  }
  
  class Visitor extends Actor {

    var visited = HashMap[Long, State]()
    
    def receive = {
      
      case state : State => {

      visited.get(stateToLong(state)) match {
        case Some(s) => {

          if (state.states.size < s.states.size) {
            visited.put(stateToLong(state), state)
            sender ! false
          } else {
            // the current path is shorter
            sender ! true
          }

        }
        case None => {
          visited.put(stateToLong(state), state)
          sender ! false
        }
      }
    }
    }
  }

  class Collector extends Actor {

    var shortest: Option[State] = None

    def receive = {
      case s: State => {
        Console.println("shortest")
        if( shortest.isDefined ){
          if( shortest.get.states.size >= s.states.size ){
            // sender() ! false
          }
          else {
            shortest = Some(s)
            Console.println("new shortest")
            Console.println(s.states.size )
            print(s)
            // sender() ! true
          }
          
        }
        else {
          shortest = Some(s)
          Console.println("new shortest")
          Console.println(s.states.size )
          print(s)
          // sender() ! true
        }
      }
      case sz : Int => {
        if( shortest.isDefined) {
          sender() ! (sz >= shortest.get.states.size)
        } 
        else {
          sender() ! false
        }
      }
    }
  }
  
       /*
        context.actorSelection("/user/controller/visitor") ! s
        val a = context.actorSelection("/user/controller/collector")
        val timeout = Timeout(5, TimeUnit.SECONDS)
        val future = ask(a, s)(timeout).mapTo[Boolean]
        val resp = Await.result(future, timeout.duration)
        Console.println("await:" + resp)
        */


  class Solver extends Actor {
    
    def receive = {
      case state : State => {
        // Console.print(".")
        solve(state)
      }
    }

    def isVisited(state: State): Boolean = {
      val v = context.actorSelection("/user/controller/visitor")
      val timeout = Timeout(5, TimeUnit.SECONDS)
      val future = ask(v, state)(timeout).mapTo[Boolean]
      Await.result(future, timeout.duration)
    }

    def solve( state: State) = {

      isVisited(state)

      // is this safe?
      if(isSolution(state)) {
          Console.println("solved...")
          solved(state)
          print(state)
      } 
      else {

          val ele = elevatorFloor(state)
          val mvs = generateMoves(ele, state.pairs.size)

          // for each move
          for (mv <- mvs) {
            up(state, mv) match {
              case Some(up) => {
                if (isVisited(up)) {
                  // state = checked + 1
                } else if (up.isLoop()) {
                  // checked = checked + 1
                } else if (isTooLong(up)) {
                  // ichecked = checked + 1
                } else if (!up.isSafe()) {
                  // checked = checked + 1
                } else {
                  enqueue(up)
                }
              }
              case None => {
                // checked = checked + 1
              }
            }

            down(state, mv) match {
              case Some(down) => {
                if (isVisited(down)) {
                  // checked = checked + 1
                } else if (down.isLoop()) {
                  // checked = checked + 1
                } else if (isTooLong(down)) {
                  // checked = checked + 1
                } else if (!down.isSafe()) {
                  // checked = checked + 1
                } else {
                  enqueue(down)
                }
              }
              case None => {
                //checked = checked + 1
              }
            }
          }
      }

    }
    
    def solved( state : State ) = {
      val a = context.actorSelection("/user/controller/collector")
      a ! state
    }

    def isTooLong( s : State ) : Boolean = {
      val a = context.actorSelection("/user/controller/collector")
      val timeout = Timeout(5, TimeUnit.SECONDS)
      val future = ask(a, s.states.size)(timeout).mapTo[Boolean]
      Await.result(future, timeout.duration)
    }

    def enqueue( state : State ) = {
      // 
      // Console.print(".")
      val a = context.actorSelection("/user/controller/solver")
      a ! state
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
      State(ele, pairs.toList.sortWith(equals), state.states :+ stateToLong(state))
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


    def equals(s1: State, s2: State): Boolean = {
      if (s1.ele == s2.ele) {

        val z = s1.pairs.zip(s2.pairs)
        z.forall((t: (Pair, Pair)) => { equals(t._1, t._2) })
      } else {
        false
      }
    }

    def equals(p1: Pair, p2: Pair): Boolean = {
      p1.rtg == p2.rtg && p1.chip == p2.chip
    }

  }

    def compare(p1: Pair, p2: Pair): Boolean = {

      if (p1.rtg > p2.rtg) {
        true
      } else {
        p1.chip > p2.chip
      }
    }
    
  def isSolution( state : State ) : Boolean = {
    state.pairs.forall( (p:Pair) => { p.chip == 3 && p.rtg == 3 } ) 
  }

  def print(state: State) = {
    Console.println("-" + isSolution(state))
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