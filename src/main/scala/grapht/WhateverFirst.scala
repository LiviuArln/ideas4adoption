package grapht

import org.scalacheck._
import Gen._

object WhaterverFirst {
    type Graph = Set[(Int,Int)]

    def graphGen(edges:Int, nodes:Int) = buildableOfN[Set[(Int,Int)],(Int,Int)](edges, {pairGen(nodes)}).map{ edges => 
        val trueEdges = edges.filter { case (o, d) => o != d }
        trueEdges ++ trueEdges.map { case (o, d) => d -> o }
    }


    def pairGen(order:Int) = for {
        origin <- chooseNum(0,order-1)
        destination <- chooseNum(0,order-1)
    } yield (origin, destination)


    
    trait Traverser {
        def push(n:Int)

        def next:Int

        def isEmpty:Boolean    

        def processEnd:Unit

        def preCollect(n:Int):Unit

        def postCollect(n:Int):Unit

        def g:Graph

        val adiacencies = g.groupBy(_._1).map { case (origin, links) => 
                origin -> links.map(_._2)
        }.toMap

        def traverse(start:Int) = {
            push(start)

            while(!isEmpty) {
                val nx = next
                preCollect(nx)
                adiacencies.get(nx).map(_.foreach(push))
                postCollect(nx)
            }
            processEnd
        } 
    }

    class GraphT(gt:Graph) {
        val g = gt
    }

    
    trait BasicContainer {
        def remove:Int
        def add(n:Int):Unit
        def contains(n:Int):Boolean
        def isEmpty:Boolean
        
        val visited = scala.collection.mutable.Set[Int]()

        def push(n:Int) {
            if(!(contains(n) || visited.contains(n))) add(n)
        }

        def next = {
            val n = remove
            visited += n
            n
        }
    }
    
    trait ClassicContainer extends BasicContainer {
        def nodes:scala.collection.AbstractSeq[Int]

        def contains(n:Int) = nodes.contains(n)
        def isEmpty = nodes.isEmpty
    }

    trait BreadthFirstContainer extends ClassicContainer {        
        val nodes = scala.collection.mutable.Queue[Int]()
        
        def remove = nodes.dequeue
        def add(n:Int) = nodes.enqueue(n)
    }

    trait DepthFirstContainer extends ClassicContainer {
        val nodes = scala.collection.mutable.Stack[Int]()
        
        def remove = nodes.pop
        def add(n:Int) = nodes.push(n)
    }

    trait PrintNodeProcessor {
        var count = 0
        def beforeCollect(n:Int) = {
            count+=1
        }

        def preCollect(n:Int) {}

        def postCollect = {
            println(count)
            println("--end--")
        }
    }

    trait CoreOrderContainer {
        def coreFirst:Boolean
        def stackLike:Boolean

        object ConnectivityAndIndexOrdering extends Ordering[(Int,Int,Int)] {
            def compare(a:(Int,Int,Int), b:(Int,Int,Int)) = {
                val coreMultiplier = if(coreFirst) -1 else 1
                val stackLikeMultiplier = if(stackLike) -1 else 1
                val connectivity = (a._1 compare b._1) * coreMultiplier
                if(connectivity == 0) (a._2 compare b._2) * stackLikeMultiplier
                else connectivity
            }
        }

        val priotizedNodes = scala.collection.mutable.TreeSet[(Int,Int,Int)]()(ConnectivityAndIndexOrdering)
        var currentIndex = 0

        val visited = scala.collection.mutable.Set[Int]()

        def push(n:Int) {
            if(!visited.contains(n)) {
                priotizedNodes.find(_._3 == n).map { case pn @ (connectivity, index, node) =>
                    priotizedNodes.remove(pn)
                    priotizedNodes.add((connectivity+1, index, node))
                }.getOrElse {
                    currentIndex += 1
                    priotizedNodes.add((1, currentIndex, n))
                }
            }
        }

        def next = {
            val pn = priotizedNodes.head
            priotizedNodes.remove(pn)
            visited.add(pn._3)
            pn._3
        }

        def isEmpty = priotizedNodes.isEmpty
    }

    trait CoreFirst extends CoreOrderContainer {
        def coreFirst = true
        def stackLike = false
    }

    trait PeriferyFirst extends CoreOrderContainer {
        def coreFirst = false
        def stackLike = true
    }

    trait LayerHighlighProcessor {
        var currentLayer = 0
        var nextLayerCut = 0
        var currentIndex = 0
        var changeLayer = false


        def preCollect(n:Int) = {
            println(n)
            changeLayer = currentIndex == nextLayerCut 
        }

        def nodes:scala.collection.AbstractSeq[Int]

        def postCollect(n:Int)  = {
            if(changeLayer) { 
                nextLayerCut = currentIndex + nodes.size
                println("Finished processing layer " + currentLayer)
                currentLayer += 1
            }
            currentIndex += 1
        }

        def processEnd = {
            println("--end--  " + currentIndex)
        }
    }

    trait CoreInternalsProcessor {
        def priotizedNodes:scala.collection.mutable.TreeSet[(Int,Int,Int)]
        
        def preCollect(n:Int) = {}

        def postCollect(n:Int) = {
            if(!priotizedNodes.isEmpty) {
                val nextProcessing = priotizedNodes.head
                println(nextProcessing._3 +"\t: "+nextProcessing._1)
                if(nextProcessing._1 > max._2) {
                    max = (nextProcessing._3, nextProcessing._1)
                }
            }
        }

        def processEnd = {
            println("--end--")
        }

        var max = (0,0);
    }


    
}



import Math._
import scala.collection.mutable.Map

object F {
    val start = (0,1)

    type Graph = Set[(Int,Int)]

    def size(g:Graph) = g.map(n => max(n._1,n._2)).fold(0)(max) + 1

    def fuse(peak: (Int,Int), graph:Graph) = {
        val nextValue = size(graph)
        val (firstNewPeak, firstPivot) = pivot(peak, graph, nextValue)
        val (secondNewPeak, secondPivot) = pivot(peak.swap, graph, nextValue * 2 - 1)
        (firstNewPeak, secondNewPeak) -> (firstPivot ++ secondPivot)
    }

    def pivot(axis:(Int, Int) , graph:Graph, nextValue: Int):(Int, Graph) = {
        val (center, base) = axis
        val translate = new Translator(center, nextValue)
        
        val newGraph = graph.map { case (x:Int,y:Int) => 
           translate(x) -> translate(y)
        }

        val newBase = translate(base)

        newBase -> (graph ++ newGraph + (base -> newBase))
    }

    class Translator(center:Int, start:Int) {
        var next = start
        val translations = Map(center -> center)

        def apply(i:Int) = translations.getOrElseUpdate(i, withAdvance)

        def withAdvance = {
            next += 1
            next - 1
        }
    }

    def go(steps: Int) = {
        var i = steps;
        var p = (start, Set(start))
        while(i > 0) {
            p = fuse(p._1, p._2)
            println("peaks: " + p._1)
            println("graph: " + p._2)
            println("nodes: " + size(p._2))
            println("edges: " + p._2.size)
            println
            
            i = i - 1
        }
        println("end... ")
        p._2
    }

     
    
}
    

object TestWhatever extends App {
    import WhaterverFirst._
    
    def bidirectional(g:Graph):Graph = g ++ g.map{_.swap} 

    def complement(g:Graph):Graph = {
        val order = F.size(g) - 1
        val complete = for {
            origin <- 0 to order
            destination <- 0 to order
            if(origin != destination)
        } yield (origin, destination)
        complete.toSet -- g
    } 
     

    //val n = 100
    //val maxE = n * n / 10
    //val g = graphGen(Math.sqrt(maxE).toInt*10,n).sample.get.toSet  

    val g = complement(bidirectional(F.go(4)))

    val tg = new GraphT(g) with Traverser with CoreFirst with CoreInternalsProcessor
    tg.traverse(0)
    (new GraphT(g) with Traverser with CoreFirst with CoreInternalsProcessor).traverse(tg.max._1)
    println(tg.max)
}