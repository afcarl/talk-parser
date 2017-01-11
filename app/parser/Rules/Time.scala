package parser.Rules

import scala.collection.mutable.ArrayBuffer

object Time {

  val rules = ArrayBuffer(
    //TODO sentence
    """sentence -> {kai/phrase}{time_sentence} | {time_sentence}{kai/phrase} | {kai/phrase}{time_sentence}{kai/phrase}""",
    """time_sentence -> {datetime}{kai/phrase}{time_sentence} | {datetime}""",

    //TODO space
    """datetime -> {date}{time} | {date} {time} | {date} | {time}""",
    """date -> {year}년 {month}월 {date_of_month}일 | {month}월 {date_of_month}일 | {date_of_month}일 | 오늘 | 내일 | 모레 | 어제 | 그제""",
    """year -> {kai/num}""",
    """month -> {kai/num}""",
    """date_of_month -> {kai/num}""",
    """time -> {오전_오후} {time_value}시 | {오전_오후}{time_value}시 | {time_value}시 | {time_value}{am_pm}""",
    """time_value -> {kai/num} """,

    """am_pm -> pm | am """,
    """오전_오후 -> 오전 | 오후"""
  )

}
