package koolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    var methods: List[MethodDecl] = List.empty

    prog match {
      case p: Program =>
        // we need a list of all MethodDecl
        for(c <- p.classes)
          methods = methods ::: c.methods

        // check soundness of all overriden methods
        methods foreach checkMethod
        // now process all statements
        p.main.stats foreach tcStat
        for(m <- methods) {
          m.stats foreach tcStat
        }
    }

    def checkMethod(decl: MethodDecl): Unit = {
     decl.getSymbol.overridden match {
        case None => // Ok, no problem
        case Some(mSym: MethodSymbol) =>
          getMethodDecl(methods, mSym) match {
            case None => ??? // Error overrides method that doesn't exist - already checked!
            case Some(m) =>
              // check nr of arguments
              if (m.args.length != decl.args.length) {
                error(decl.id.value+" overrides previous definition from "+m.position+" with a different number of parameters.", decl)
                TError
              } else {
                // check that arg types match
                for (i <- 0 until m.args.length) {
                  val refType = m.args(i).getSymbol.getType
                  val argType = decl.args(i).getSymbol.getType
                  if(!(argType isSubTypeOf refType))
                    error("Formal type in overriding method "+decl.id.value+" does not match type in overridden method.", decl.args(i))
                }
              }

              // check that retType match
              if(!(decl.retType.getType isSubTypeOf m.retType.getType))
                error("Type error: Expected: "+m.retType.getType+", found: "+decl.retType.getType, decl.retType)
          }
      }

      // check for all methods that retExpression is a subtype of retType!
      val returnedType = tcExpr(decl.retExpr)
      if(!(returnedType isSubTypeOf decl.retType.getType)) {
        error("Type error: Expected: "+decl.retType.getType+", found: "+returnedType, decl.retExpr)
        //error("The return statement of '"+decl.id.value+"' does not match the declared return type of the method at " + decl.retType.position + ". Found " + returnedType + ", expected " + decl.retType.getType + ".", decl.retExpr)
        decl.retType.setType(TError)
      }

    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs: ExprTree, rhs: ExprTree) =>
          logical(lhs, rhs)
          expr.setType(TBoolean)
          TBoolean
        case Or(lhs: ExprTree, rhs: ExprTree) =>
          logical(lhs, rhs)
          expr.setType(TBoolean)
          TBoolean
        case Plus(lhs: ExprTree, rhs: ExprTree) =>
          val plusTpe: Type = handlePlus(lhs, rhs)
          expr.setType(plusTpe)
          plusTpe
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
          mathematical(lhs, rhs, TInt)
          expr.setType(TInt)
          TInt
        case Mod(lhs: ExprTree, rhs: ExprTree) =>
          mathematical(lhs, rhs, TInt)
          expr.setType(TInt)
          TInt
        case Times(lhs: ExprTree, rhs: ExprTree) =>
          mathematical(lhs, rhs, TInt)
          expr.setType(TInt)
          TInt
        case Div(lhs: ExprTree, rhs: ExprTree) =>
          mathematical(lhs, rhs, TInt)
          expr.setType(TInt)
          TInt
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          mathematical(lhs, rhs, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
          handleEquals(lhs, rhs) match {
            case TBoolean =>
              expr.setType(TBoolean)
              TBoolean
            case _ =>
              error("Incompatible types in equality: " + lhs.getType + ", " + rhs.getType, expr)
              TError
          }
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
          tcExpr(index, TInt)
          tcExpr(arr, TIntArray)
          expr.setType(TInt)
          TInt
        case ArrayLength(arr: ExprTree) =>
          tcExpr(arr, TIntArray)
          expr.setType(TInt)
          TInt
        case m: MethodCall => handleMethCall(m)

        case IntLit(_) => expr.setType(TInt); TInt
        case StringLit(_) => expr.setType(TString); TString
        case True() => expr.setType(TBoolean); TBoolean
        case False() => expr.setType(TBoolean); TBoolean
        case ident: Identifier => ident.getType

        case t: This => t.getType
        case NewIntArray(size: ExprTree) =>
          tcExpr(size, TInt)
          expr.setType(TIntArray)
          TIntArray
        case New(ident: Identifier) =>
          tcExpr(ident)
          expr.setType(ident.getType)
          expr.getType
        case Not(expr: ExprTree) =>
          tcExpr(expr, TBoolean)
          expr.setType(TBoolean)
          TBoolean
      }

      expected.toList.length

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        if(expected.toList.length == 1)
          error("Type error: Expected: " + expected.head + ", found: " + tpe, expr)
        else
          error("Type error: Expected one of the following types: " + expected.toList.mkString(", ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats: List[StatTree]) => stats foreach tcStat
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          tcExpr(expr, TBoolean)
          tcStat(thn)
          els match {
            case Some(s) => tcStat(s)
            case None    =>
          }
        case While(expr: ExprTree, stat: StatTree) =>
          tcExpr(expr, TBoolean)
          tcStat(stat)
        case Println(expr: ExprTree) =>
          tcExpr(expr, TInt, TString, TBoolean)

        case Assign(assignee: Identifier, expr: ExprTree) =>
          assignee.getType match {
            case TIntArray => tcExpr(expr, TIntArray)
            case _ => tcExpr(expr, assignee.getType)
          }

        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          tcExpr(id, TIntArray)
          tcExpr(index, TInt)
          tcExpr(expr, TInt)
      }
    }

    def logical(lhs: ExprTree, rhs: ExprTree): Type = {
      tcExpr(lhs, TBoolean)
      tcExpr(rhs, TBoolean)
      TBoolean
    }

    def handlePlus(lhs: ExprTree, rhs: ExprTree): Type = {
      val lType = tcExpr(lhs, TInt, TString)
      val rType = tcExpr(rhs, TInt, TString)
      lhs.setType(lType)
      rhs.setType(rType)
      if(lType == TString || rType == TString) TString else TInt
   }

    def mathematical(lhs: ExprTree, rhs: ExprTree, expected: Type): Type = {
      tcExpr(lhs, TInt)
      tcExpr(rhs, TInt)
      expected
    }

    def handleEquals(lhs: ExprTree, rhs: ExprTree): Type = {
      val lType = tcExpr(lhs)
      val rType = tcExpr(rhs)
      lhs.setType(lType)
      rhs.setType(rType)
      if(lType != rType) TError else TBoolean
    }

    def handleMethCall(m: MethodCall): Type = {
      val theClass: ClassSymbol = m.obj match {
        case i: Identifier =>
          i.getSymbol.getType match {
            case TObject(c) => c
            case _ =>
              error(i.value + " is not a class", i)
              new ClassSymbol("empty").setType(TError)
          }
        case New(i: Identifier) =>
          i.getSymbol.getType match {
            case TObject(c) => c
            case _ =>
              error(i.value + " is not a class", i)
              new ClassSymbol("empty").setType(TError)
          }
        case t: This =>
          t.getSymbol.getType match {
            case TObject(c) => c
            case _ =>
              // this can never happen :)
              error("'this' is not a class", m.obj)
              new ClassSymbol("empty").setType(TError)
          }
        case method: MethodCall =>
          tcExpr(method) match {
            case TObject(c: ClassSymbol) => c
            case s =>
              error("Type error: Expected: Object, found: "+s, method.meth)
              new ClassSymbol("empty").setType(TError)
          }
        case _ =>
          val tpe = tcExpr(m.obj)
          error("Type error: Expected: Object, found: " + tpe, m.obj)
          new ClassSymbol("empty").setType(TError)
      }

      checkMethDecl(theClass, m)
    }

    def getMethodDecl(methList: List[MethodDecl], lookingFor: MethodSymbol): Option[MethodDecl] = {
      if(methList.isEmpty)
        None
      else if(methList.head.getSymbol.id == lookingFor.id)
        Option(methList.head)
      else
        getMethodDecl(methList.tail, lookingFor)
    }

    def checkMethDecl(theClass: ClassSymbol, call: MethodCall): Type = {
      theClass.lookupMethod(call.meth.value) match {
        case Some(mSym: MethodSymbol) =>
          getMethodDecl(methods, mSym) match {
            case None => ??? // Cannot happen? Method has a symbol but does not exist????
            case Some(method: MethodDecl) =>
              // check nr of arguments
              if (method.args.length != call.args.length) {
                error("Wrong number of arguments for method "+method.id.value+".", call.meth)
                TError
              } else {
                // check that arg types match
                for (i <- 0 until method.args.length) {
                  val refType = method.args(i).getSymbol.getType
                  val argType = tcExpr(call.args(i))
                  if(!(argType isSubTypeOf refType))
                    error("Type error: Expected: "+refType+", found: "+argType, call.args(i))
                }
                call.setType(method.retType.getType)
                method.retType.getType
              }
          }
        case None => // need to look in parent if parent exist
          theClass.parent match {
            case None =>
              error("Method "+call.meth.value+" does not exist in class Object", call)
              TError
            case Some(p: ClassSymbol) => checkMethDecl(p, call)
          }
      }
    }

    //terminateIfErrors
    prog
  }

}
