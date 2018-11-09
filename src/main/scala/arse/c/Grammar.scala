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

  val op = S("[~!%^&|*-+=<>,/]+")
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

  val global: Parser[Global] = P(typedef | structdef | enumdef | fundef | vardef)
  val unit = global *

  val typedef = type_def("typedef" ~ typ ~ name ~ ";")
  val structdef = struct_def("struct" ?~ name ?~ struct ~ ";")
  val enumdef = enum_def("enum" ?~ name ?~ enum ~ ";")

  val init = "=" ~ expr
  val vardef = var_def(typ ?~ name ?~ init.? ~ ";")

  val block: Parser[Block] = Block("{" ~ ret(Nil) ~ "}")
  val block_option = (block map { Some(_) }) | None(";")

  val param = Param(typ ~ name)
  val params = param ~* ","
  val fundef = fun_def(typ ?~ name ?~ "(" ~ params ~ ")" ~ block_option)

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
