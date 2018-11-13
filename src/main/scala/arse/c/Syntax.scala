package arse.c

case class Field(typ: Type, name: String)
case class Param(typ: Type, name: String)

sealed trait Global

case class TypeDef(typ: Type, name: String) extends Global
case class StructDef(name: String, fields: List[Field]) extends Global
case class UnionDef(name: String, fields: List[Field]) extends Global
case class EnumDef(name: String, cases: List[String]) extends Global

case class VarDef(typ: Type, name: String, init: Option[Expr]) extends Global with Stmt
case class FunDef(ret: Type, name: String, params: List[Param], body: Option[Block]) extends Global

sealed trait Type
sealed trait BaseType extends Type
sealed trait TypeName extends Type { def name: String }

case object Void extends BaseType

case object SChar extends BaseType
case object SShort extends BaseType
case object SInt extends BaseType
case object SLong extends BaseType

case object UChar extends BaseType
case object UShort extends BaseType
case object UInt extends BaseType
case object ULong extends BaseType

case class TypedefName(name: String) extends TypeName
case class StructName(name: String) extends TypeName
case class UnionName(name: String) extends TypeName
case class EnumName(name: String) extends TypeName

case class Ptr(typ: Type) extends Type

case class StructType(fields: List[Field]) extends Type
case class UnionType(cases: List[Field]) extends Type
case class EnumType(consts: List[String]) extends Type

sealed trait Expr

case class Id(name: String) extends Expr
case class Lit(value: Any) extends Expr

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

case class FunCall(name: String, args: List[Expr]) extends Expr // no function pointers

case class Init(values: List[(Option[String], Expr)]) extends Expr // { .field = value } or { value }

case class Block(stmts: List[Stmt])

sealed trait Stmt
case class Atomic(expr: Expr) extends Stmt
case class Return(expr: Option[Expr]) extends Stmt
case class If(test: Expr, left: Block, right: Option[Block]) extends Stmt
case class While(test: Expr, body: Block) extends Stmt
case class For(init: Expr, test: Expr, inc: Expr, body: Block) extends Stmt

object Dot extends ((Expr, String) => Expr) {
  def apply(expr: Expr, field: String) = {
    Lookup(Ref(expr), field)
  }
}

object Syntax {
  def modifies(expr: Expr): Set[Id] = expr match {
    case UnOp("++", arg) => modifies(arg)
    case UnOp("--", arg) => modifies(arg)
    case BinOp(op, id: Id, arg2) if Operators.low contains op => Set(id)
    case BinOp(op, arg1, arg2) => Set()
    case Question(test, left, right) => modifies(test) ++ modifies(left) ++ modifies(right)
    case Cast(typ, expr) => modifies(expr)
    case Lookup(expr, field) => modifies(expr)
    case Index(expr, index) => modifies(expr) ++ modifies(index)
    case Ref(expr) => modifies(expr)
    case DeRef(expr) => modifies(expr)
    case FunCall(name, args) => Set() ++ (args flatMap modifies)
    case Init(values) => Set() ++ (values flatMap { case (_, expr) => modifies(expr) })
    case _ => Set()
  }

  def has_sideeffect(expr: Expr): Boolean = expr match {
    case UnOp("++", arg) => has_sideeffect(arg)
    case UnOp("--", arg) => has_sideeffect(arg)
    case BinOp(op, arg1, arg2) if Operators.low contains op => true
    case BinOp(op, arg1, arg2) => false
    case Question(test, left, right) => has_sideeffect(test) || has_sideeffect(left) || has_sideeffect(right)
    case Cast(typ, expr) => has_sideeffect(expr)
    case Lookup(expr, field) => has_sideeffect(expr)
    case Index(expr, index) => has_sideeffect(expr) || has_sideeffect(index)
    case Ref(expr) => has_sideeffect(expr)
    case DeRef(expr) => has_sideeffect(expr)
    case FunCall(name, args) => true // XXX: approximation
    case Init(values) => (values exists { case (_, expr) => has_sideeffect(expr) })
    case _ => false
  }

  def modifies(block: Block): Set[Id] = block match {
    case Block(stmts) => Set() ++ (stmts flatMap modifies)
  }

  def modifies(stmt: Stmt): Set[Id] = stmt match {
    case _: VarDef => Set()
    case Atomic(expr) => modifies(expr)
    case Return(None) => Set()
    case Return(Some(expr)) => modifies(expr)
    case If(test, left, None) => modifies(test) ++ modifies(left)
    case If(test, left, Some(right)) => modifies(test) ++ modifies(left) ++ modifies(right)
    case While(test, body) => modifies(test) ++ modifies(body)
    case For(init, test, inc, body) => modifies(init) ++ modifies(test) ++ modifies(inc) ++ modifies(body)
  }
}
