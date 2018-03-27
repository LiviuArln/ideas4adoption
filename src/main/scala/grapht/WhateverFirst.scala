package grapht

import org.scalacheck._
import Gen._
import breeze.linalg.DenseVector

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


    
    abstract class Traverser(g:Graph) {
        def push(n:Int)

        def next:Int

        def isEmpty:Boolean    

        def processEnd:Unit

        def preCollect(n:Int):Unit

        def postCollect(n:Int):Unit

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
        def stackLike = true
    }

    trait PeriferyFirst extends CoreOrderContainer {
        def coreFirst = false
        def stackLike = true
    }

    def test = {
        val n = 100
        val maxE = n * (n-1)
        val g = graphGen(Math.sqrt(maxE).toInt*10,n).sample.get.toSet  

        //println(g)
        //(new Traverser(g) with BreadthFirstContainer with PrintNodeProcessor).traverse(0)
        //println("-----------")
        //(new Traverser(g) with DepthFirstContainer with PrintNodeProcessor).traverse(0)
        //println("-----------")
        (new Traverser(g) with CoreFirst with CoreInternalsProcessor).traverse(0)
        println("-----------")
        (new Traverser(g) with PeriferyFirst with CoreInternalsProcessor).traverse(0)

        //(new Traverser(g) with BreadthFirstContainer with LayerHighlighProcessor).traverse(0)
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
            if(!priotizedNodes.isEmpty) println(priotizedNodes.head._3 +"\t: "+priotizedNodes.head._1)
        }

        def processEnd = {
            println("--end--")
        }
    }


    
}