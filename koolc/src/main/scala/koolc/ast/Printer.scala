package koolc
package ast

import Trees._

object Printer {
  var sb: StringBuilder = new StringBuilder
  var indentation = 0
  var printSymID = false

  def apply(t: Tree, printSymbols: Boolean = false): String = {
    printSymID = printSymbols
    sb.append("object ")
    addMainID(t)
    openBlock
    addIndentation
    sb.append("def main(): Unit =")
    openBlock
    addStatTrees(t)
    closeBlock
    closeBlock
    addClasses(t)
    sb.toString
  }

  def addMainID(t: Tree): Unit = {
    //t.main --- TODO: change to this instead!!!!!!!!
    t match {
      case Program(main,_) =>
        main match {
          case MainObject(Identifier(id),_) =>
            sb.append(id)
            if(printSymID)
              sb.append("#"+main.getSymbol.id)
          case _ => ???
        }
      case _ => ???
    }
  }

  def addClasses(t: Tree): Unit = {
    t match {
      case Program(_,list) => list foreach addClass
      case _ => ???
    }
  }

  def addStatTrees(t: Tree): Unit = {
    val statementList = t match {
      case Program(MainObject(_,list),_) => list foreach addStatement
      case _ =>
    }
  }

  def addClass(c: ClassDecl): Unit = {
    sb.append("\nclass ")
    c match {
      case ClassDecl(ident, parent, vars, methods) =>
        val id = ident match {
          case Identifier(id) => id
        }
        sb.append(id)
        if(printSymID)
          sb.append("#"+c.getSymbol.id)
        //sb.append("~"+c.getSymbol.getType)
        parent match {
          case Some(ident: Identifier) =>
            sb.append(" extends ")
            addExpr(ident)
          case _ =>
        }
        openBlock
        vars foreach addVar
        methods foreach addMethod
        closeBlock
      case _ => ??? // bad format
    }
  }

  def addVar(v: VarDecl): Unit = {
    addIndentation
    sb.append("var ")
    v match {
      case VarDecl(tpe, Identifier(id)) =>
        sb.append(id)
        if(printSymID)
          sb.append("#"+v.getSymbol.id)
        //sb.append("~"+v.getSymbol.getType)
        sb.append(": ")
        addType(tpe)
        sb. append(";\n")
      case _ => ???
    }
  }

  def addMethod(m: MethodDecl): Unit = {
    addIndentation
    sb.append("def ")
    m match {
      case MethodDecl(tpe, Identifier(id), args, vars, stats, retExpr) =>
        sb.append(id)
        if(printSymID)
          sb.append("#"+m.getSymbol.id)
        sb.append("(")
        //println("method: "+id)
        for(a <- args) {
          addFormal(a)
          sb.append(", ")
        }
        //println("added all args")
        if(!args.isEmpty)
          sb = sb.dropRight(2)
        sb.append(") : ")
        addType(tpe)
        sb.append(" =")
        openBlock
        vars foreach addVar
        stats foreach addStatement
        addIndentation
        sb.append("return ")
        addExpr(retExpr)
        sb.append(";\n")
        closeBlock
      //println("done with "+id)
    }
  }

  def addFormal(f: Formal): Unit = {
    f match {
      case Formal(tpe, ident: Identifier) =>
        val id = ident match {
          case Identifier(id) => id
        }
        sb.append(id)
        if(printSymID)
          sb.append("#"+f.getSymbol.id)
        //sb.append("~"+ident.getSymbol.getType)
        sb.append(": ")
        addType(tpe);
      case _ => ???
    }
  }

  def addType(t: TypeTree): Unit = {
    t match {
      case BooleanType() => sb.append("Bool")
      case IntType() => sb.append("Int")
      case IntArrayType() => sb.append("Int[]")
      case StringType() => sb.append("String")
      case Identifier(identifier) => sb.append(identifier) //+"~"+t.getType)
    }
  }

  def addStatement(t: StatTree): Unit = {
    t match {

      case Block(statTree) =>
        openBlock
        statTree foreach addStatement
        closeBlock

      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        addIndentation
        val hasBlock = thn match {
          case Block(_) => true
          case _ => false
        }
        sb.append("if(")
        addExpr(expr)
        sb.append(")")
        if(!hasBlock) openBlock
        addStatement(thn)
        if(!hasBlock) closeBlock
        els match {
          case Some(s) =>
            sb = sb.dropRight(1)
            sb.append(" else")
            if(!hasBlock) openBlock
            addStatement(s)
            if(!hasBlock) closeBlock
          case None => // nothing?
        }

      case While(expr: ExprTree, stat: StatTree) =>
        addIndentation
        sb.append("while(")
        addExpr(expr)
        sb.append(")")
        addStatement(stat)

      case Println(expr) =>
        addIndentation
        sb.append("println(")
        addExpr(expr)
        sb.append(");\n")

      case Assign(ident: Identifier, expr: ExprTree) =>
        addIndentation
        ident match {
          case Identifier(id) =>
            sb.append(id)
            if(printSymID)
              sb.append("#"+ident.getSymbol.id)
          case _ => ???
        }
        //sb.append("~"+ident.getSymbol.getType)
        sb.append(" = ")
        addExpr(expr)
        sb.append(";\n")

      case ArrayAssign(ident: Identifier, index: ExprTree, expr: ExprTree) =>
        addIndentation
        ident match {
          case Identifier(id) =>
            sb.append(id)
            if(printSymID)
              sb.append("#"+ident.getSymbol.id)
            //sb.append("~"+ident.getSymbol.getType)
          case _ => ???
        }
        sb.append("[")
        addExpr(index)
        sb.append("] = ")
        addExpr(expr)
        sb.append(";\n")
    }
  }

  def addExpr(expr: ExprTree): Unit = {
    expr match {
      case And(lhs: ExprTree, rhs: ExprTree) => add("&&", lhs, rhs)
      case Or(lhs: ExprTree, rhs: ExprTree) => add("||", lhs, rhs)
      case Plus(lhs: ExprTree, rhs: ExprTree) => add("+", lhs, rhs)
      case Minus(lhs: ExprTree, rhs: ExprTree) => add("-", lhs, rhs)
      case Times(lhs: ExprTree, rhs: ExprTree) => add("*", lhs, rhs)
      case Div(lhs: ExprTree, rhs: ExprTree) => add("/", lhs, rhs)
      case Mod(lhs: ExprTree, rhs: ExprTree) => add("%", lhs, rhs)
      case LessThan(lhs: ExprTree, rhs: ExprTree) => addExpr(lhs); sb.append(" < "); addExpr(rhs)
      case Equals(lhs: ExprTree, rhs: ExprTree) => addExpr(lhs); sb.append(" == "); addExpr(rhs)
      case ArrayLength(id) => addExpr(id); sb.append(".length")
      case IntLit(value) => sb.append(value)
      case StringLit(value) => sb.append("\""+value+"\"")
      case True() => sb.append("true")
      case False() => sb.append("false")
      case ident: Identifier =>
        //println("identifier: "+ident)
        //println(sb.toString())
        ident match {
          case Identifier(value) =>
            sb.append(value)
            if(printSymID) {
              sb.append("#" + ident.getSymbol.id)
              //sb.append("~" + ident.getType)
            }
          case _ => ??? // CANNOT HAPPEN
        }
      case This() => sb.append("this"); //~"+expr.getType)
      case Not(expr: ExprTree) => sb.append("!"); sb.append("("); addExpr(expr); sb.append(")")

      case ArrayRead(id, index) =>
        addExpr(id)
        sb.append("[")
        addExpr(index)
        sb.append("]")

      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
        addExpr(obj)
        sb.append(".")
        addExpr(meth)
        sb.append("(")
        for(expr <- args) {
          addExpr(expr)
          sb.append(", ")
        }
        if(!args.isEmpty)
          sb = sb.dropRight(2)
        sb.append(")")

      case NewIntArray(size: ExprTree) =>
        sb.append("new Int[")
        addExpr(size)
        sb.append("]")

      case New(tpe: Identifier) => {
        tpe match {
          case Identifier(id) =>
            sb.append("new "+id)
            if(printSymID)
              sb.append("#" + tpe.getSymbol.id)
            //sb.append("~"+tpe.getSymbol.getType)
            sb.append("()")
          case _ => ???
        }
      }
    }
  }

  def add(str: String, lhs: ExprTree, rhs: ExprTree): Unit = {
    sb.append("(")
    addExpr(lhs)
    sb.append(")")
    sb.append(" "+str+" ")
    sb.append("(")
    addExpr(rhs)
    sb.append(")")
  }
  def addIndentation = sb.append("  " * indentation)
  def openBlock = {
    sb.append(" {\n")
    indentation += 1
  }
  def closeBlock = {
    indentation -= 1
    addIndentation
    sb.append("}\n")
  }
}

