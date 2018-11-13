package arse

package object c {
  def parse(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val text = source.mkString
    object context extends Context
    object whitespace extends arse.Whitespace("\\s*")
    object grammar extends arse.c.Grammar(context)
    val in = arse.input(text)(whitespace)
    val res = grammar.unit.$.parse(in)
    (res, context)
  }
}