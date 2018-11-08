package arse.c

case class Field(typ: Type, name: String)

sealed trait Global

case class TypeDef(typ: Type, name: String) extends Global

case class StructDef(name: String, fields: Option[List[Field]]) extends Global
case class UnionDef(name: String, cases: Option[List[Field]]) extends Global
case class EnumDef(name: String, consts: Option[List[String]]) extends Global

case class VarDef(typ: Type, name: String, init: Option[Expr]) extends Global
case class FunDef(ret: Type, name: String, args: List[Type], body: Option[Block]) extends Global

sealed trait Type
case object Void extends Type
case object SInt extends Type // size in bytes
case object UInt extends Type // size in bytes

case class Ptr(typ: Type) extends Type

case class TypeName(name: String) extends Type
case class StructName(name: String) extends Type
case class UnionName(name: String) extends Type
case class EnumName(name: String) extends Type

case class StructType(fields: List[Field]) extends Type
case class UnionType(cases: List[Type]) extends Type
case class EnumType(consts: List[String]) extends Type

sealed trait Expr

case class Id(name: String) extends Expr with Type
case class Const(value: Any) extends Expr

case class UnOp(op: String, arg: Expr) extends Expr // op arg
case class BinOp(op: String, arg1: Expr, arg2: Expr) extends Expr // arg1 op arg2
case class Question(test: Expr, left: Expr, right: Expr) extends Expr // test ? left : right

case class SizeOfType(typ: Type) extends Expr
case class SizeOfExpr(expr: Expr) extends Expr
case class Cast(typ: Type, expr: Expr) extends Expr

case class Lookup(expr: Expr, field: String) extends Expr // expr -> field
case class Index(expr: Expr, index: Expr) extends Expr // expr[index]
case class Ref(expr: Expr) extends Expr // &expr
case class DeRef(expr: Expr) extends Expr // *expr

case class FunCall(name: String, args: List[Expr]) // no function pointers

case class Init(values: List[(Option[String], Expr)]) extends Expr // { .field = value } or { value }

case class Block(stmts: List[Stmt])

sealed trait Stmt
case class Atomic(expr: Expr) extends Stmt
case class Return(expr: Option[Expr]) extends Stmt
case class If(test: Expr, left: Block, right: Block) extends Stmt
case class While(test: Expr, body: Block) extends Stmt
case class For(init: Expr, test: Expr, inc: Expr, body: Block) extends Stmt

object Dot extends ((Expr, String) => Expr) {
  def apply(expr: Expr, field: String) = {
    Lookup(Ref(expr), field)
  }
}