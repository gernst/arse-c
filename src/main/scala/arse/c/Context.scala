package arse.c

import scala.collection.mutable

class Context {
  val typedefs = mutable.Map[String, Type]()
  val structs = mutable.Map[String, Type]()
  val enums = mutable.Map[String, Type]()
  val unions = mutable.Map[String, Type]()
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

  object union_def extends ((String, UnionType) => UnionDef) {
    def apply(name: String, typ: UnionType) = {
      structs(name) = typ
      UnionDef(name, typ.cases)
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
