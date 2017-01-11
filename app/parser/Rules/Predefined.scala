package parser.Rules

import parser.{NTSymbol, Rule, Symbol, TSymbol}

import scala.collection.mutable.ArrayBuffer

object Predefined {

  val Prefix = "kai"

  def predefinedSymbolName(name: String) = "%s/%s".format(Prefix, name)

  val PHRASE = NTSymbol(predefinedSymbolName("phrase"))
  val SPACE = TSymbol(predefinedSymbolName("space"), "\\s")
  val WORD = NTSymbol(predefinedSymbolName("word"))
  val CHAR_GEN = NTSymbol(predefinedSymbolName("char_gen"))
  val CHAR = NTSymbol(predefinedSymbolName("char"))
  val ALPHABET = TSymbol(predefinedSymbolName("alphabet"), "^[a-zA-Z]$")
  val KOREAN_CHAR = TSymbol(predefinedSymbolName("korean_char"), "^[ㄱ-ㅎㅏ-ㅣ가-힣]$")
  val DOT = TSymbol(predefinedSymbolName("dot"), "\\.")
  val APOSTROPHE = TSymbol(predefinedSymbolName("apostrophe"), "'")

  val NUM = NTSymbol(predefinedSymbolName("num"))
  val DIGIT_GEN = NTSymbol(predefinedSymbolName("digit_gen"))
  val DIGIT = TSymbol(predefinedSymbolName("digit"), "^[0-9]$")

  val Symbols = ArrayBuffer[Symbol](
    PHRASE, SPACE,
    WORD, CHAR_GEN, ALPHABET, KOREAN_CHAR, APOSTROPHE, DOT,
    NUM, DIGIT_GEN, DIGIT
  )

  val Rules = ArrayBuffer[Rule](

    /* Phrase */
    Rule(PHRASE, SPACE, PHRASE),
    Rule(PHRASE, PHRASE, SPACE),
    Rule(PHRASE, SPACE),
    Rule(PHRASE, WORD),
    Rule(PHRASE, WORD, SPACE, PHRASE),

    /* Word */
    Rule(WORD, CHAR_GEN),
    Rule(CHAR_GEN, CHAR, CHAR_GEN),
    Rule(CHAR_GEN, CHAR),
    Rule(CHAR, ALPHABET),
    Rule(CHAR, KOREAN_CHAR),
    Rule(CHAR, DOT),
    Rule(CHAR, APOSTROPHE),

    /* Number */
    Rule(NUM, DIGIT_GEN),
    Rule(DIGIT_GEN, DIGIT, DIGIT_GEN),
    Rule(DIGIT_GEN, DIGIT)
  )

}
