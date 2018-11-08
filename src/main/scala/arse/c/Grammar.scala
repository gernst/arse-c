package arse.c

import arse._
import arse.implicits._

class Grammar {
  object context {
    var typedefs: Map[String, Type] = Map()
    var structs: Map[String, Type] = Map()
    var unions: Map[String, Type] = Map()
    var enums: Map[String, Type] = Map()
  }

  import context._

  val op = S("[~!%^&|*-+=<>,/]+")
  val name = S("[a-zA-Z_][a-zA-Z_0-9]*")
  val names = name ~* ","

  val id = Id(name)
  val const = (int | string) map Const // XXX: proper excaping of string literals

  val type_spec = Void("void") | SInt("int") // "char", "short", "int", "long", "float", "double", "signed", "unsigned")

  val type_union = "union" ~ unions(name)
  val type_struct = "struct" ~ structs(name)
  val type_enum = "enum" ~ enums(name)
  val type_def = typedefs(name filter (n => typedefs contains n))
  val typ: Parser[Type] = type_union | type_struct | type_def | type_spec

  object app extends ((String, List[Expr]) => Expr) {
    def apply(op: String, args: List[Expr]) = args match {
      case List(arg) => UnOp(op, arg)
      case List(arg1, arg2) => BinOp(op, arg1, arg2)
    }
  }

  val expr_low: Parser[Expr] = M(cond_expr, op, app, Operators.low)
  val expr_high: Parser[Expr] = M(expr_cast, op, app, Operators.high)

  val primary_expr = id | const

  val expr_cast: Parser[Expr] = P(Cast("(" ?~ typ ~ ")" ~ expr_cast))
  val expr_parens = ("(" ~ expr_low ~ ")")
  // val expr_unary = ???

  val cond_expr: Parser[Expr] = P(expr_high ~ ("?" ~ expr_low ~ ":" ~ cond_expr ?) map {
    case a ~ None => a
    case a ~ Some(b ~ c) => Question(a, b, c)
  })

  val expr = expr_low
  val exprs = expr ~+ ","

  val global: Parser[Global] = P(typedef | structdef | uniondef | enumdef | vardef | fundef)
  val unit = global *

  val field = Field(typ ~ name)
  val fields = (field ~ ";") *

  val typedef = TypeDef("typedef" ~ typ ~ name ~ ";")
  val structdef = StructDef("struct" ~ name ~ ("{" ~ fields ~ "}").? ~ ";")
  val uniondef = UnionDef("union" ~ name ~ ("{" ~ fields ~ "}").? ~ ";")
  val enumdef = EnumDef("enum" ~ name ~ ("{" ~ names ~ "}").? ~ ";")

  val init = "=" ~ expr
  val vardef = VarDef(typ ~ name ~ init.?)

  val fundecl_arg = typ <~ name.?
  val fundecl_args = fundecl_arg ~* ","
  val block: Parser[Block] = Block("{" ~ ret(Nil) ~ "}")
  val fundef = FunDef(typ ~ name ~ "(" ~ fundecl_args ~ ")" ~ block.?)

  /*
sealed trait Type
  val void = Void
  val sint = SInt(size)
  val uint = UInt(size)
  val ptr = Ptr(typ)

  val typename = TypeName(name)
  val structname = StructName(name)
  val unionname = UnionName(name)
  val enumname = EnumName(name)

  val structtype = StructType(fields[Field])
  val uniontype = UnionType(cases[Type])

sealed trait Expr

  val id = Id(name)
  val const = Const(value)

  val unop = UnOp(op ~ arg)
  val binop = BinOp(op ~ arg1 ~ arg2)
  val question = Question(test ~ left ~ right)

  val sizeoftype = SizeOfType(typ)
  val sizeofexpr = SizeOfExpr(expr)
  val cast = Cast(typ ~ expr)

  val lookup = Lookup(expr ~ field)
  val index = Index(expr ~ index)
  val ref = Ref(expr)
  val deref = DeRef(expr)

  val funcall = FunCall(name ~ args[Expr]) // no function pointers

  val init = Init(values[(Option[String] ~ Expr)])

  val block = Block(stmts[Stmt])

sealed trait Stmt
  val atomic = Atomic(expr)
  val return = Return(expr[Expr])
  val if = If(test ~ left ~ right)
  val while = While(test ~ body)
  val for = For(init ~ test ~ inc ~ body)
   */
}
