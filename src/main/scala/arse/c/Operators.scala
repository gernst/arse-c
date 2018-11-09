package arse.c

import arse._

object Operators {
  object typ extends Syntax[String] {
    val postfix_ops = Map("*" -> 0)
    val prefix_ops = Map()
    val infix_ops = Map()
  }
  
  object high extends Syntax[String] {
    val postfix_ops = Map()
    val prefix_ops = Map(
      "++" -> 14,
      "--" -> 14,
      "&" -> 14,
      "*" -> 14,
      "+" -> 14,
      "-" -> 14,
      "~" -> 14,
      "!" -> 14)
    val infix_ops = Map(
      "*" -> (Left, 13),
      "/" -> (Left, 13),
      "%" -> (Left, 13),
      "+" -> (Left, 12),
      "-" -> (Left, 12),
      "<<" -> (Left, 11),
      ">>" -> (Left, 11),
      "<" -> (Left, 10),
      "<=" -> (Left, 10),
      ">=" -> (Left, 10),
      ">" -> (Left, 10),
      "==" -> (Left, 9),
      "!=" -> (Left, 9),
      "&" -> (Left, 8),
      "^" -> (Left, 7),
      "|" -> (Left, 6),
      "&&" -> (Left, 5),
      "||" -> (Left, 4))
  }

  object low extends Syntax[String] {
    val postfix_ops = Map()
    val prefix_ops = Map()
    val infix_ops = Map(
      "=" -> (Right, 2),
      "*=" -> (Right, 2),
      "/=" -> (Right, 2),
      "%=" -> (Right, 2),
      "+=" -> (Right, 2),
      "-=" -> (Right, 2),
      "<<=" -> (Right, 2),
      ">>=" -> (Right, 2),
      "&=" -> (Right, 2),
      "^=" -> (Right, 2),
      "|=" -> (Right, 2),
      "," -> (Left, 1))
  }

}