package arse.c

import arse._
import arse.implicits._

class Grammar(val context: Context) {
  import context._ // see at the end

  val low_op = L(Operators.low.ops.sorted.reverse: _*) // long identifiers first
  val high_op = L(Operators.high.ops.sorted.reverse: _*)
  val name = S("[a-zA-Z_][a-zA-Z_0-9]*")
  val names = name ~* ","

  val id = P(Id(name))
  val const = P(int map Lit)
  val type_spec = Void("void") | SChar("char") | SShort("short") | SInt("int") | SLong("long") | UChar("unsigned\\s+char") | UShort("unsigned\\s+short") | UInt("unsigned\\s+int") | ULong("unsigned\\s+long")

  object ptr extends ((String, List[Type]) => Type) {
    def apply(op: String, args: List[Type]) = args match {
      case List(arg) if op == "*" => Ptr(arg)
      case List(arg1, arg2) => ???
    }
  }

  val typ: Parser[Type] = M(type_primary, L("*"), ptr, Operators.typ)

  val field = Field(typ ~ name)
  val fields = (field ~ ";") *
  val struct = StructType("{" ~ fields ~ "}")
  val union = UnionType("{" ~ fields ~ "}")
  val enum = EnumType("{" ~ names ~ "}")

  val type_struct = "struct" ~ (struct | StructName(name))
  val type_enum = "enum" ~ (enum | EnumName(name))
  val type_df = TypedefName(name filter typedefs.contains)
  val type_primary = type_struct | type_enum | type_df | type_spec

  object app extends ((String, List[Expr]) => Expr) {
    def apply(op: String, args: List[Expr]) = args match {
      case List(arg) if op == "&" => Ref(arg)
      case List(arg) if op == "*" => DeRef(arg)
      case List(arg) => UnOp(op, arg)
      case List(arg1, arg2) => BinOp(op, arg1, arg2)
    }
  }

  val expr_low: Parser[Expr] = M(cond_expr, low_op, app, Operators.low)
  val expr_high: Parser[Expr] = M(expr_cast, high_op, app, Operators.high)

  val expr = P(expr_low)
  val exprs = P(expr ~* ",")

  val funcall = FunCall(name ?~ "(" ~ exprs ~ ")")
  val expr_primary: Parser[Expr] = funcall | id | const

  val index = "[" ~ expr ~ "]" map { index => (base: Expr) => Index(base, index) }
  val dot = "." ~ name map { field => (base: Expr) => Dot(base, field) }
  val arrow = "->" ~ name map { field => (base: Expr) => Lookup(base, field) }
  val post: Parser[Expr => Expr] = index | dot | arrow
  val expr_postfix = post.foldLeft(expr_primary)((e, f) => f(e))

  val expr_cast: Parser[Expr] = P(Cast("(" ?~ typ ~ ")" ~ expr_cast)) | expr_postfix
  val expr_parens = ("(" ~ expr_low ~ ")")
  // val expr_unary = ???

  val cond_expr: Parser[Expr] = P(expr_high ~ ("?" ~ expr_low ~ ":" ~ cond_expr ?) map {
    case a ~ None => a
    case a ~ Some(b ~ c) => Question(a, b, c)
  })

  val global: Parser[Global] = P(typedef | structdef | uniondef | enumdef | fundef | vardef)
  val unit = global *

  val typedef = type_def("typedef" ~ typ ~ name ~ ";")
  val uniondef = union_def("union" ?~ name ?~ union ~ ";")
  val structdef = struct_def("struct" ?~ name ?~ struct ~ ";")
  val enumdef = enum_def("enum" ?~ name ?~ enum ~ ";")

  val init = "=" ~ expr
  val vardef = var_def(typ ?~ name ?~ init.? ~ ";")

  val block: Parser[Block] = P(Block("{" ~ stmts ~ "}"))
  val block_or_stmt: Parser[Block] = P(Block("{" ~ stmts ~ "}" | stmt1 | (stmt0 ~ ";")))

  val _return = Return("return" ~ expr.? ~ ";")
  val _if = If("if" ~ expr_parens ~ block_or_stmt ~ ("else" ~ block_or_stmt).?)
  val _while = While("while" ~ expr_parens ~ block_or_stmt)
  val _for_head = "(" ~ expr ~ ";" ~ expr ~ ";" ~ expr ~ ")"
  val _for = For("for" ~ _for_head ~ block_or_stmt)
  val atomic = Atomic(expr ~ ";")

  val stmt = _return | _if | _while | vardef | atomic
  val stmt0 = ret(Nil)
  val stmt1 = stmt map { List(_) }
  val stmts = stmt *

  val block_option = (block map { Some(_) }) | None(";")

  val param = Param(typ ~ name)
  val params = param ~* ","
  val funsig = P(typ ?~ name ?~ "(" ~ params ~ ")")
  val fundef = P(fun_def(funsig ~ block_option))
}
