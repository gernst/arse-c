package arse.c.test

import scala.io.Source

object Grammar {
    def main(args: Array[String]) {
    val (res, _) = arse.c.parse("src/test/c/test.c")
    res map println
  }
}