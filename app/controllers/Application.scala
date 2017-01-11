package controllers

import parser.{Rules, TalkParser}
import TalkParser._
import play.api.mvc._
import play.twirl.api.Html

class Application extends Controller {

  def index(q: Option[String]) = Action {
    val htmlStr = q.fold("")(str =>
      extractTimeValueHtml(str)
    )
    Ok(views.html.index(Html(htmlStr)))
  }

  def extractTimeValueHtml(s: String): String = {
    val rules = TalkParser.loadRules(Rules.Time.rules)
    val parsedTrees = parseToTrees(rules, s)
    rules.foreach(println)
    parsedTrees.map { t =>
      markValues(t).map { case (symbol, v) => "<p><b>%s</b>: %s</p>".format(symbol.id, v) }.mkString("\n")
    }.map { s => "<div>\n%s\n</div>".format(s) }.mkString("<br>")
  }

}