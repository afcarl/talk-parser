package parser.Rules

import parser.{NTSymbol, Rule, TSymbol}
import parser.Rules.Predefined._

import scala.collection.mutable.ArrayBuffer

object RuleParsing {

  val NT = NTSymbol("NT")
  val NT_VALUE = NTSymbol("NT_VALUE")
  val T = NTSymbol("T")
  val TO_GEN = NTSymbol("TO_GEN")
  val TO = NTSymbol("TO")
  val RULE = NTSymbol("RULES")
  val SIMBOL = NTSymbol("S")
  val SIMBOL_GEN = NTSymbol("SIMBOL_GEN")
  val ARROW = NTSymbol("ARROW")

  val START_NT = TSymbol("", "^\\{$")
  val END_NT = TSymbol("", "^\\}$")
  val START_END_T = TSymbol("", "^\"$")
  val DASH = TSymbol("", "^-$")
  val DECREASE = TSymbol("", "^>$")
  val PIPE = TSymbol("", "^\\|$")

  val Rules = ArrayBuffer[Rule](
    Rule(RULE, NT, ARROW, TO_GEN),
    Rule(RULE, NT, ARROW, TO),
    Rule(TO_GEN, TO, PIPE, TO_GEN),
    Rule(TO_GEN, TO, PIPE, TO),
    Rule(TO, SIMBOL, SIMBOL_GEN),
    Rule(SIMBOL_GEN, SIMBOL, SIMBOL_GEN),
    Rule(SIMBOL_GEN, SIMBOL),
    Rule(TO, SIMBOL),
    Rule(SIMBOL, T),
    Rule(SIMBOL, NT),
    Rule(ARROW, DASH, DECREASE),
    Rule(NT, START_NT, NT_VALUE, END_NT),
    Rule(NT_VALUE, WORD, NUM),
    Rule(NT_VALUE, WORD),
    Rule(T, WORD),
    Rule(T, NUM)
  )
}

