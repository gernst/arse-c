package arse.c

import arse._
import arse.implicits._

class Grammar {
  object context {
    var structs: Map[String, Type] = Map()
    var enums: Map[String, Type] = Map()
    var idents: Map[String, (Type, Impl)] = Map()

    object struct_def extends ((String, List[Field]) => StructDef) {
      def apply(name: String, fields: List[Field]) = {
        structs += name -> StructType(fields)
        StructDef(name, fields)
      }
    }

    object enum_def extends ((String, List[String]) => EnumDef) {
      def apply(name: String, cases: List[String]) = {
        enums += name -> EnumType(cases)
        EnumDef(name, cases)
      }
    }

    object id_def extends ((Type, String, Impl) => IdDef) {
      def apply(typ: Type, name: String, impl: Impl) = {
        idents += name -> (typ, impl)
        IdDef(typ, name, impl)
      }
    }
  }

  import context._

  val op = S("[~!%^&|*-+=<>,/]+")
  val name = S("[a-zA-Z_][a-zA-Z_0-9]*")
  val names = name ~* ","

  val id = Id(name)
  val const = int map Const
  val type_spec = Sort(L("void", "int", "bool"))

  val type_struct = "struct" ~ structs(name)
  val type_enum = "enum" ~ enums(name)
  val typ: Parser[Type] = type_struct | type_enum | type_spec

  object app extends ((String, List[Expr]) => Expr) {
    def apply(op: String, args: List[Expr]) = args match {
      case List(arg) => UnOp(op, arg)
      case List(arg1, arg2) => BinOp(op, arg1, arg2)
    }
  }

  val expr_low: Parser[Expr] = M(cond_expr, op, app, Operators.low)
  val expr_high: Parser[Expr] = M(expr_cast, op, app, Operators.high)

  val primary_expr = id | const

  val expr_cast: Parser[Expr] = P(Cast("(" ?~ typ ~ ")" ~ expr_cast)) | primary_expr
  val expr_parens = ("(" ~ expr_low ~ ")")
  // val expr_unary = ???

  val cond_expr: Parser[Expr] = P(expr_high ~ ("?" ~ expr_low ~ ":" ~ cond_expr ?) map {
    case a ~ None => a
    case a ~ Some(b ~ c) => Question(a, b, c)
  })

  val expr = expr_low
  val exprs = expr ~+ ","

  val global: Parser[Global] = P(structdef | enumdef | iddef)
  val unit = global *

  val field = Field(typ ~ name)
  val fields = (field ~ ";") *

  val structdef = struct_def("struct" ~ name ~ "{" ~ fields ~ "}" ~ ";")
  val enumdef = enum_def("enum" ~ name ~ "{" ~ names ~ "}" ~ ";")

  val init = "=" ~ expr
  val varimpl = VarImpl(init.? ~ ";")

  val block: Parser[Block] = Block("{" ~ ret(Nil) ~ "}")
  val block_option = (block map { Some(_) }) | None(";")

  val param = Param(typ ~ name)
  val params = param ~* ","
  val funimpl = FunImpl("(" ~ params ~ ")" ~ block_option)

  val impl = funimpl | varimpl
  val iddef = id_def(typ ~ name ~ impl)

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
