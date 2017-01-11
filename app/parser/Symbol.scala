package parser


abstract class Symbol

abstract class NonTSymbol extends Symbol

case class NTSymbol(id: String, explicit: Boolean = false) extends NonTSymbol {

  override def toString = "{%s}".format(id)
}

case class SystemNTSymbol(id: String) extends NonTSymbol {

  override def toString = "{%s}".format(id)
}

case class TSymbol(id: String, regex: String) extends Symbol {

  override def toString = "'%s'".format(regex)
}
