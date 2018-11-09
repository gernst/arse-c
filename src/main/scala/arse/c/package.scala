package arse

package object c {
  def parse(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val text = source.mkString
    object whitespace extends arse.Whitespace("\\s*")
    object grammar extends arse.c.Grammar
    val in = arse.input(text)(whitespace)
    val res = grammar.unit.$.parse(in)
    (res, grammar.context)
  }
}