package arse.c.test

import scala.io.Source

object Grammar {
  def load(name: String) = {
    val source = Source.fromFile("src/test/c/" + name)
    read(name, source.mkString)
  }

  def read(name: String, text: String) = {
    object whitespace extends arse.Whitespace("\\s*")
    object grammar extends arse.c.Grammar
    val in = arse.input(text)(whitespace)
    val res = grammar.unit.$.parse(in)
    (res, grammar.context)
  }
  
  def main(args: Array[String]) {
    val (res, _) = load("test.c")
    res map println
  }
}