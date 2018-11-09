package arse.c

import scala.collection.mutable
import arse._
import arse.implicits._

class Grammar {
  object context {
    val typedefs = mutable.Map[String, Type]()
    val structs = mutable.Map[String, Type]()
    val enums = mutable.Map[String, Type]()
    val vars = mutable.Map[String, (Type, Option[Expr])]()
    val funs = mutable.Map[String, (Type, List[Param], Option[Block])]()

    object type_def extends ((Type, String) => TypeDef) {
      def apply(typ: Type, name: String) = {
        typedefs(name) = typ
        TypeDef(typ, name)
      }
    }

    object struct_def extends ((String, StructType) => StructDef) {
      def apply(name: String, typ: StructType) = {
        structs(name) = typ
        StructDef(name, typ.fields)
      }
    }

    object enum_def extends ((String, EnumType) => EnumDef) {
      def apply(name: String, typ: EnumType) = {
        enums(name) = typ
        EnumDef(name, typ.consts)
      }
    }

    object var_def extends ((Type, String, Option[Expr]) => VarDef) {
      def apply(typ: Type, name: String, init: Option[Expr]) = {
        vars(name) = (typ, init)
        VarDef(typ, name, init)
      }
    }

    object fun_def extends ((Type, String, List[Param], Option[Block]) => FunDef) {
      def apply(ret: Type, name: String, params: List[Param], body: Option[Block]) = {
        funs(name) = (ret, params, body)
        FunDef(ret, name, params, body)
      }
    }
  }

  import context._

  val low_op = L(Operators.low.ops: _*)
  val high_op = L(Operators.high.ops: _*)
  val name = S("[a-zA-Z_][a-zA-Z_0-9]*")
  val names = name ~* ","

  val id = Id(name)
  val const = int map Const
  val type_spec = Sort(L("void", "int", "bool"))

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

  val expr = expr_low
  val exprs = expr ~+ ","

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

  val global: Parser[Global] = P(typedef | structdef | enumdef | fundef | vardef)
  val unit = global *

  val typedef = type_def("typedef" ~ typ ~ name ~ ";")
  val structdef = struct_def("struct" ?~ name ?~ struct ~ ";")
  val enumdef = enum_def("enum" ?~ name ?~ enum ~ ";")

  val init = "=" ~ expr
  val vardef = var_def(typ ?~ name ?~ init.? ~ ";")

  val block: Parser[Block] = P(Block("{" ~ stmts ~ "}"))
  val block_or_stmt: Parser[Block] = P(Block("{" ~ stmts ~ "}" | (stmt1 ~ ";")))

  val _return = Return("return" ~ expr.? ~ ";")
  val _if = If("if" ~ expr_parens ~ block_or_stmt ~ ("else" ~ block_or_stmt).?)
  val _while = While("while" ~ expr_parens ~ block_or_stmt)
  val atomic = Atomic(expr ~ ";")

  val stmt = _return | _if | _while | vardef | atomic
  val stmt1 = stmt map { List(_) }
  val stmts = stmt *

  val block_option = (block map { Some(_) }) | None(";")

  val param = Param(typ ~ name)
  val params = param ~* ","
  val fundef = fun_def(typ ?~ name ?~ "(" ~ params ~ ")" ~ block_option)

  /*
  val sizeoftype = SizeOfType(typ)
  val sizeofexpr = SizeOfExpr(expr)

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
