package parser

import scala.collection._

object Utils {

  def crossProduct[T](one: Seq[T], another: Seq[T]): Seq[(T, T)] = {
    for {
      o <- one
      a <- another
    } yield (o, a)
  }

}