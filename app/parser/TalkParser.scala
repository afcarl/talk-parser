package parser

import java.util.UUID

import parser.Rules.Predefined
import scala.collection.mutable
import scala.collection.mutable._
import scala.collection.immutable.Map
import parser.Utils._

object TalkParser {

  type TwoDimArray[T] = Array[Array[T]]

  case class Node(symbol: Symbol, value: String, children: ArrayBuffer[Node] = ArrayBuffer[Node]()) {

    override def toString: String = symbol match {
      case s: NTSymbol if s.explicit =>
        var str = "(" + s.toString
        if (children.nonEmpty) {
          str += " = "
          str += children.map {
            _.toString
          }.mkString(" + ")
        }
        str += ")"
        str
      case _ => value
    }
  }

  object Node {

    def flatten(root: Node) = {
      val q = mutable.Queue[(Node, Node)]()
      root.children.foreach { node =>
        q.enqueue((node, root))
      }
      while (q.nonEmpty) {
        var (node, parent) = q.dequeue
        val children = node.children
        if (node.symbol.isInstanceOf[SystemNTSymbol]) {
          parent.children.remove(parent.children.indexOf(node))
          parent.children ++= children
        } else {
          parent = node
        }
        children.foreach(c => q.enqueue((c, parent)))
      }
    }
  }

  case class Cell(nodes: ArrayBuffer[Node] = ArrayBuffer[Node]()) {

    override def toString = nodes.mkString(", ")
  }

  def parse(ruleStrs: ArrayBuffer[String], s: String): ArrayBuffer[ArrayBuffer[(NTSymbol, String)]] = {
    val rules = TalkParser.loadRules(ruleStrs)
    //    rules.foreach(println)
    val parsedTrees = parseToTrees(rules, s)
    parsedTrees.map { t =>
      markValues(t)
    }
  }

  def preprocess(s: String) = {
    s.trim
  }

  def parseToTrees(rules: ArrayBuffer[Rule], rawString: String): ArrayBuffer[Node] = {
    val refinedStr = preprocess(rawString)
    val bnfRules = rules.flatMap(toBNFRule)

    val units = getUnitRelations(bnfRules)

    val dpArray = parseByCyk4Bnf(refinedStr, bnfRules, units)
    if(dpArray.length > 0) {
      val cell = dpArray(dpArray.length - 1)(0)
      cell.nodes.map { n =>
        Node.flatten(n)
        n
      }
    } else {
      ArrayBuffer[Node]()
    }
  }

  def extractValues(node: Node): ArrayBuffer[(NTSymbol, String)] = {
    val res = ArrayBuffer[(NTSymbol, String)]()
    val q = Queue[Node]()
    q.enqueue(node)
    while (q.nonEmpty) {
      val n = q.dequeue
      n.symbol match {
        case s: NTSymbol if s.explicit => res += n.symbol.asInstanceOf[NTSymbol] -> n.value
        case _ =>
      }
      n.children.foreach(q.enqueue(_))
    }
    res
  }

  def markValues(node: Node): ArrayBuffer[(NTSymbol, String)] = {
    val res = ArrayBuffer[(NTSymbol, String)]()
    val q = Queue[Node]()
    q.enqueue(node)
    while (q.nonEmpty) {
      val n = q.dequeue
      n.symbol match {
        case s: NTSymbol if s.explicit => res += s.asInstanceOf[NTSymbol] -> markedValue(n)
        case _ =>
      }
      n.children.foreach(q.enqueue(_))
    }
    res
  }

  def markedValue(node: Node): String = {
    node.symbol match {
      case s: NTSymbol if s.explicit =>
        node.children.map { c => c.symbol match {
          case s: NTSymbol if s.explicit => "<mark>%s</mark>".format(c.value)
          case _ => c.value
        }}.mkString("")
      case _ => ""
    }
  }

  def getUnitRelations(bnfRules: ArrayBuffer[BNFRule]) = {
    val symbols = bnfRules.toSet.flatMap(getSymbols)
    symbols.map(s => s -> unitRelations(s, bnfRules)).toMap[Symbol, mutable.Set[Symbol]]
  }

  def loadRules(ruleStrs: ArrayBuffer[String]) = {
    ruleStrs.flatMap(r => parseRule(r, Predefined.Symbols)) ++= Predefined.Rules
    //    ruleStrs.flatMap(r => parseRuleByCyk(RuleParsingRules.Rules, r))
  }

  /**
    * A => {B}cd | {E}{F} | g
    */
  private def parseRule(ruleStr: String, predefinedSymbols: ArrayBuffer[Symbol]): ArrayBuffer[Rule] = {
    val rules = ArrayBuffer[Rule]()
    try {
      var splitedStr = ruleStr.trim.split("->")
      if (splitedStr.size != 2) rules
      else {
        val (left, right) = (splitedStr.head.trim, splitedStr.last.trim)
        splitedStr = right.split("\\|").map(_.trim)
        for (s <- splitedStr) {
          val symbols = ArrayBuffer[Symbol]()
          val it = s.iterator
          while (it.hasNext) {
            var next = it.next
            if (next == '{') {
              var nt = ""
              next = it.next
              while (it.hasNext && next != '}') {
                nt += next
                next = it.next
              }
              symbols += findOrCreateNTSymbol(predefinedSymbols, nt)
            } else {
              symbols += findOrCreateTSymbol(predefinedSymbols, next.toString)
            }
          }
          val from = findOrCreateNTSymbol(predefinedSymbols, left)
          rules += Rule(from, symbols: _*)
        }
      }
      rules
    } catch {
      case e: Exception => {
        println("invalid rule!")
        rules
      }
    }
  }

  private def findOrCreateNTSymbol(predefinedSymbols: ArrayBuffer[Symbol], name: String) = predefinedSymbols.find {
    case s: NTSymbol => s.id == name
    case _ => false
  }.getOrElse(NTSymbol(name, true))

  private def findOrCreateTSymbol(predefinedSymbols: ArrayBuffer[Symbol], name: String) = predefinedSymbols.find {
    case s: TSymbol => s.regex == name
    case _ => false
  }.getOrElse(TSymbol(name.toString, name))


//  private def parseRuleByCyk(parsingRules: ArrayBuffer[Rule], ruleStr: String, predefinedSymbols: ArrayBuffer[Symbol]): ArrayBuffer[Rule] = {
//
//    val bnfRules = parsingRules.flatMap(toBNFRule)
//    val unitRelations = getUnitRelations(bnfRules)
//
//    val refinedStr = preprocess(ruleStr)
//    val dpArray = parseByCyk4Bnf(refinedStr, bnfRules, unitRelations)
//    val cell = dpArray(dpArray.length - 1)(0)
//
//    if (cell.nodes.isEmpty || !cell.nodes.exists(_.symbol == RULE)) ArrayBuffer[Rule]()
//    else {
//      val root = cell.nodes.head
//      Node.flatten(root)
//      var from = None: Option[Symbol]
//      val toList = ArrayBuffer[ArrayBuffer[Symbol]]()
//      val q = mutable.Queue[Node]()
//      q.enqueue(root)
//      while (q.nonEmpty) {
//        val n = q.dequeue()
//        if (n.children.size == 3 && n.children(1).symbol == ARROW) {
//          val child = n.children(0)
//          from = Option(findOrCreateNTSymbol(predefinedSymbols, child.children(1).value))
//        }
//        else if (n.symbol == TO) {
//          val to: ArrayBuffer[Symbol] = {
//            if (n.symbol.isInstanceOf[TSymbol])
//              ArrayBuffer[Symbol](findOrCreateTSymbol(predefinedSymbols, n.value))
//            else if (n.children.size == 3 && unitRelations(n.children(1).symbol).contains(NT_VALUE))
//              ArrayBuffer[Symbol](findOrCreateNTSymbol(predefinedSymbols, n.children(1).value))
//            else {
//              n.children.map {
//                case child if child.children.size == 3 && child.children(1).symbol == NT_VALUE => findOrCreateNTSymbol(predefinedSymbols, child.children(1).value)
//                case child => findOrCreateTSymbol(predefinedSymbols, child.value)
//              }
//            }
//          }
//          toList += to
//        }
//        else n.children.foreach(q.enqueue(_))
//      }
//      (from, toList) match {
//        case (Some(f), to) if to.nonEmpty => to.map(Rule(f, _: _*))
//        case _ => ArrayBuffer[Rule]()
//      }
//    }
//  }

  def printDpArray(twoDimArray: TwoDimArray[Cell]) = {
    print(twoDimArray.map(_.mkString(" | ")).mkString("\n"))
  }


  def getSymbols(rule: BNFRule): Set[Symbol] = {
    val symbols = mutable.Set[Symbol]()
    symbols.add(rule.from)
    rule.to.foreach { r =>
      symbols.add(r)
    }
    symbols
  }

  def toBNFRule(rule: Rule): ArrayBuffer[BNFRule] = {
    if (rule.to.isEmpty) ArrayBuffer[BNFRule]()
    else if (rule.to.size == 1) ArrayBuffer[BNFRule](BNFRule(rule.from, rule.to.head))
    else {
      val bnfRules = ArrayBuffer[BNFRule]()
      val currIt = rule.to.iterator
      val nextIt = rule.to.iterator
      nextIt.next
      var currNTSymbol = rule.from
      while (nextIt.hasNext) {
        val curr = currIt.next
        val next = nextIt.next
        if (nextIt.hasNext) {
          val nextNTSymbol = SystemNTSymbol(UUID.randomUUID().toString)
          bnfRules += BNFRule(currNTSymbol, curr, nextNTSymbol)
          currNTSymbol = nextNTSymbol
        } else {
          bnfRules += BNFRule(currNTSymbol, curr, next)
        }
      }
      bnfRules
    }
  }

  def unitRelations(symbol: Symbol, rules: ArrayBuffer[BNFRule]): mutable.Set[Symbol] = {
    val res = mutable.Set[Symbol]()
    res += symbol

    val q = mutable.Queue[Symbol]()
    q ++= rules.filter(r => r.to.length == 1 && r.to.contains(symbol)).map(_.from)

    while (q.nonEmpty) {
      val symbol = q.dequeue()
      if (!res.contains(symbol)) {
        res += symbol
        q ++= rules.filter(r => r.to.length == 1 && r.to.exists {
          case s: NTSymbol => s == symbol
          case _ => false
        }).map(_.from)
      }
    }
    res
  }

  def getTSymbol(c: Char, rules: ArrayBuffer[BNFRule]) = {
    rules.flatMap { r => r.to.find {
      case s: TSymbol => s.regex.r.findFirstIn(c.toString).isDefined
      case _ => false
    }
    }.toSet
  }

  def parseByCyk4Bnf(targetStr: String, rules: ArrayBuffer[BNFRule], units: Map[Symbol, mutable.Set[Symbol]]): TwoDimArray[Cell] = {
    val len = targetStr.length
    val dpArray = Array.fill[Cell](len, len)(Cell())

    for (colIdx <- 0 until len) {
      val c = targetStr(colIdx)
      getTSymbol(c, rules).foreach { ts =>
        dpArray(0)(colIdx).nodes += Node(ts, c.toString, ArrayBuffer[Node]())
      }
    }

    for (rowIdx <- 1 until len) {
      for (colIdx <- 0 until len - rowIdx) {
        for (k <- 1 to rowIdx) {
          val (x1, y1) = (rowIdx - k, colIdx)
          val (x2, y2) = (k - 1, colIdx + rowIdx - k + 1)
          var nodes = crossProduct[Node](dpArray(x1)(y1).nodes, dpArray(x2)(y2).nodes)
            .flatMap { case (n1, n2) => crossProduct[Symbol](units(n1.symbol).toSeq, units(n2.symbol).toSeq)
              .flatMap { case (s1, s2) =>
                matchedNTSymbol(rules, s1, s2).map { r =>
                  val c1 = if (n1.symbol == s1) n1 else Node(s1, n1.value, ArrayBuffer[Node](n1))
                  val c2 = if (n2.symbol == s2) n2 else Node(s2, n2.value, ArrayBuffer[Node](n2))
                  Node(r.from, c1.value + c2.value, ArrayBuffer[Node](c1, c2))
                }
              }
            }.toSet
          dpArray(rowIdx)(colIdx).nodes ++= nodes
        }
      }
    }
    dpArray
  }

  def matchedNTSymbol(rules: Seq[BNFRule], one: Symbol, another: Symbol) = {
    rules.filter(r => r.to.length == 2 && r.to.head == one && r.to.last == another)
  }

}