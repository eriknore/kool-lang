package koolc
package analyzer

import ast.Trees._

object SymIDPrinter {
  var sb: StringBuilder = new StringBuilder
  def apply(t: Tree): String = {
    sb.append("Program(")
    addMainID(t)
    sb.append(",")
    addStatTrees(t)
    sb.append(",")
    addClasses(t)
    sb.append(")")
    sb.toString
  }
  
  def addMainID(t: Tree): Unit = {
    t match {
      case Program(main: MainObject,_) =>
        main match {
          case MainObject(ident,_) => 
            ident match {
              case Identifier(id) =>
                sb.append("MainObject#"+main.getSymbol.id)
                sb.append("(")
                addIdentWithSym(ident)
            }
        }
      case _ => ???
    } 
  }
  
  def addClasses(t: Tree): Unit = {
    t match {
      case Program(_,list) =>
        sb.append("List(")
        if(!list.isEmpty) {
          addClass(list.head)
          for (c <- list.tail) {
            sb.append(",")
            addClass(c)
          }
        }
        sb.append(")")
      case _ => ???
    }
  }
  
  def addStatTrees(t: Tree): Unit = {
    val statementList = t match {
      case Program(MainObject(_,list),_) => addStats(list); sb.append(")")
      case _ => 
    }
  }
  
  
  def addClass(c: ClassDecl): Unit = {
    sb.append("ClassDecl#"+c.getSymbol.id+"(")
    c match {
      case ClassDecl(ident, parent, vars, methods) =>
        addIdentWithSym(ident)
        sb.append(",")
        parent match {
          case Some(ident) => 
            sb.append("Some(")
            addIdentWithSym(ident)
            sb.append(")")
          case _ => sb.append(parent)
        }
        sb.append(",")
        addVars(vars)
        sb.append(",")
        addMethods(methods)
        sb.append(")")
        
      case _ => ??? // bad format
    }
  }
  
  def addVar(v: VarDecl): Unit = {
    
    sb.append("VarDecl#"+v.getSymbol.id+"(")
    v match {
      case VarDecl(tpe, ident) => 
        addType(tpe)
        sb.append(",")
        ident match {
          case i: Identifier => addIdentWithSym(i)
          case _ => sb.append(ident)
        }
      case _ => ???
    }
    sb.append(")")
  }
  
  def addMethod(m: MethodDecl): Unit = {
    
    sb.append("MethodDecl#"+m.getSymbol.id+"(")
    m match {
      case MethodDecl(tpe, ident, args, vars, stats, retExpr) =>
        addType(tpe)
        sb.append(",")
        addIdentWithSym(ident)
        sb.append(",")
        addFormals(args)
        sb.append(",")
        addVars(vars)
        sb.append(",")
        addStats(stats)
        sb.append(",")
        addExpr(retExpr)
    }
    sb.append(")")
  }
  
  def addFormal(f: Formal): Unit = {
    sb.append("Formal#"+f.getSymbol.id+"(")
    f match {
      case Formal(tpe, ident) =>
        addType(tpe)
        sb.append(",")
        addIdentWithSym(ident)
        
      case _ => ???
    }
    sb.append(")")
  }
  
  def addType(t: TypeTree): Unit = {
    t match {
      case i: Identifier => addIdentWithSym(i)
      case _ => sb.append(t)
    }
  }
  
  def addStatement(t: StatTree): Unit = {
    t match {
      
      case Block(statTree) =>
        sb.append("Block(")
        addStats(statTree)
        sb.append(")")
        
      
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        sb.append("If(")
        addExpr(expr)
        sb.append(",")
        addStatement(thn) 
        els match {
          case Some(s) =>
            sb.append(",Some(") 
            addStatement(s)
            sb.append(")")
             
          case None => sb.append(",None")
        }
        sb.append(")")
      
      case While(expr: ExprTree, stat: StatTree) =>
        sb.append("While(")
        addExpr(expr)
        sb.append(",")
        addStatement(stat)
        sb.append(")")
      
      case Println(expr) =>
        sb.append("Println(")
        addExpr(expr)
        sb.append(")")
      
      case Assign(ident: Identifier, expr: ExprTree) =>
        sb.append("Assign(")
        addIdentWithSym(ident)
        sb.append(",")
        addExpr(expr)
        sb.append(")")
      
      case ArrayAssign(ident: Identifier, index: ExprTree, expr: ExprTree) =>
        sb.append("ArrayAssign(")
        addExpr(ident)
        sb.append(",")
        addExpr(index)
        sb.append(",")
        addExpr(expr)
        sb.append(")")
    }
  }
  
  def addExpr(expr: ExprTree): Unit = {
    expr match {
      case And(lhs: ExprTree, rhs: ExprTree) => add("And", lhs, rhs)
      case Or(lhs: ExprTree, rhs: ExprTree) => add("Or", lhs, rhs)
      case Plus(lhs: ExprTree, rhs: ExprTree) => add("Plus", lhs, rhs)
      case Minus(lhs: ExprTree, rhs: ExprTree) => add("Minus", lhs, rhs)
      case Times(lhs: ExprTree, rhs: ExprTree) => add("Times", lhs, rhs)
      case Div(lhs: ExprTree, rhs: ExprTree) => add("Div", lhs, rhs)
      case Mod(lhs: ExprTree, rhs: ExprTree) => add("Mod", lhs, rhs)
      case LessThan(lhs: ExprTree, rhs: ExprTree) => add("LessThan",lhs,rhs)
      case Equals(lhs: ExprTree, rhs: ExprTree) => add("Equals",lhs,rhs)
      case ArrayLength(id) => sb.append("ArrayLength("); addExpr(id); sb.append(")")
      case IntLit(value) => sb.append(expr)
      case StringLit(value) => sb.append(expr)
      case True() => sb.append(expr)
      case False() => sb.append(expr)
      case ident: Identifier => addIdentWithSym(ident)
      case t: This => sb.append("This#"+t.getSymbol.id+"()")
      
      case Not(expr: ExprTree) => sb.append("Not("); addExpr(expr); sb.append(")")

      case ArrayRead(id, index) =>
        sb.append("ArrayRead(")
        addExpr(id)
        sb.append(",")
        addExpr(index)
        sb.append(")")
         
      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
        sb.append("MethodCall(")
        obj match {
          case i: Identifier => addIdentWithSym(i)
          case t: This => sb.append("This#"+t.getSymbol.id+"()")
          case New(i: Identifier) => 
            sb.append("New(")
            addIdentWithSym(i)
            sb.append(")")
          case _ => addExpr(obj)
        }
        sb.append(",")
        //println(expr)
        sb.append("Identifier("+meth.value+")")
        sb.append(",List(")
        if(!args.isEmpty) {
          addExpr(args.head)
          for(a <- args.tail) {
            sb.append(",")
            addExpr(a)
          }
        }
        sb.append("))")
      
      case NewIntArray(size: ExprTree) => 
        sb.append("NewIntArray(")
        addExpr(size)
        sb.append(")")
      
      case New(tpe: Identifier) =>
        sb.append("New(")
        addIdentWithSym(tpe)
        sb.append(")")
    }
  }
  
  def addVars(vars: List[VarDecl]): Unit = {
    sb.append("List(")
    if(!vars.isEmpty) {
      addVar(vars.head)
      for(v <- vars.tail) {
        sb.append(",")
        addVar(v)
      }
    }
    sb.append(")")
  }
  
  def addFormals(formals: List[Formal]): Unit = {
    sb.append("List(")
    if(!formals.isEmpty) {
      addFormal(formals.head)
      for(f <- formals.tail) {
        sb.append(",")
        addFormal(f)
      }
    }
    sb.append(")")
  }
  
  def addStats(stats: List[StatTree]): Unit = {
    sb.append("List(")
    if(!stats.isEmpty) {
      addStatement(stats.head)
      for(s <- stats.tail) {
        sb.append(",")
        addStatement(s)
      }
    }
    sb.append(")")
  }
  
  def addMethods(methods: List[MethodDecl]): Unit = {
    sb.append("List(")
    if(!methods.isEmpty) {
      addMethod(methods.head)
      for(s <- methods.tail) {
        sb.append(",")
        addMethod(s)
      }
    }
    sb.append(")")
  }
  
  def add(str: String, lhs: ExprTree, rhs: ExprTree): Unit = {
    sb.append(str+"(")
    addExpr(lhs)
    sb.append(",")
    addExpr(rhs)
    sb.append(")")
  }
  
  def addIdentWithSym(i: Identifier): Unit = {
    i match {
      case Identifier(id) =>
        sb.append("Identifier#"+i.getSymbol.id)
        sb.append("("+id+")")
    }
  }
 
}
