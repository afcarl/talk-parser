package controllers

import parser.{Rules, TalkParser}
import TalkParser._
import play.api.mvc._
import play.twirl.api.Html

class Application extends Controller {

  def index(q: String) = Action {
    Ok(views.html.index(Html(extractTimeValueHtml(q))))
  }

  def extractTimeValueHtml(s: String): String = {
    val rules = TalkParser.loadRules(Rules.Time.rules)
    val parsedTrees = parseToTrees(rules, s)
        rules.foreach(println)
    parsedTrees.map { t =>
      markValues(t).map { case (symbol, v) => "<p>%s</p>".format(v) }.mkString("\n")
    }.map { s => "<div>\n%s\n</div>".format(s)}.mkString("\n")
  }

}