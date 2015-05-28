package koolc
package analyzer

import utils._
import ast.Trees._
import Types._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  var global = new GlobalScope
  var currentPos = 0
  var classList: List[ClassDecl] = List.empty
  var processed: List[ClassDecl] = List.empty
  var declaredVars: Array[Option[Identifier]] = Array.empty
  var usedVars: Array[Boolean] = Array.empty

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def getClassBySym(sym: ClassSymbol, classes: List[ClassDecl]): Option[ClassDecl] = {
      if(classes.isEmpty)
        None
      else {
        classes.head match {
          case ClassDecl(ident,_,_,_) =>
            if(ident.getSymbol == sym)
              Option(classes.head)
            else
              getClassBySym(sym, classes.tail)
        }
      }
    }

    def getClassByID(id: String, classes: List[ClassDecl]): Option[ClassDecl] = {
      if(classes.isEmpty)
        None
      else {
        classes.head match {
          case ClassDecl(Identifier(name),_,_,_) =>
            if(name == id)
              Option(classes.head)
            else
              getClassByID(id, classes.tail)
        }
      }
    }

    def addClass(c: ClassDecl): Unit = {
      if(!processed.contains(c) || !c.hasSymbol) {
        c match {
          case ClassDecl(ident, parent, vars, methods) =>
            val id = ident match {
              case Identifier(id) => id

            }
            // does this class already exist?
            global.lookupClass(id) match {
              case Some(s) =>
                if(s.id == 0) // i.e. MainObject
                  error("Class "+id+" has the same name as the main class.", ident)
                else
                  error("Class "+id+" is defined more than once. First definition here: "+s.position, ident)
                c.setSymbol(new ClassSymbol("error")).setPos(c.file, Position.encode(c.line, c.col))
                c.getSymbol.setType(TError)
              case None => // if no, add a new symbol
                c.setSymbol(new ClassSymbol(id).setPos(c.file, Position.encode(c.line, c.col)))
                c.getSymbol.setType(new TObject(c.getSymbol))
            }

            // add to global classes
            global.classes = global.classes + (id -> c.getSymbol)
            // tag the identifer with class symbol
            ident.setSymbol(c.getSymbol)
            ident.setType(c.getSymbol.getType)
            processed = c :: processed;

            parent match {
              case Some(ident) =>
                ident match {
                  case Identifier(pid) =>
                    // check for cycle!
                    val p:ClassDecl = getClassByID(pid, classList) match {
                      case None =>
                        error("Class " + id + " extends class " + pid + " which is not defined.", ident)
                        val temp: ClassDecl = new ClassDecl(new Identifier("error"), None, List.empty, List.empty)
                        temp.setSymbol(new ClassSymbol("error"))
                        temp
                      case Some(s) =>
                        if(!processed.contains(s)) {
                          addClass(s)
                          processed = s :: processed
                        }
                        s
                    }
                    c.getSymbol.parent = Option(p.getSymbol)
                    ident.setSymbol(p.getSymbol)
                    ident.setType(p.getSymbol.getType)

                }
              case None => // do nothing
            }
        }
      }
    }

    def removeElementFromList(toRemove: ClassDecl, list: List[ClassDecl]): List[ClassDecl] = {
      var toReturn: List[ClassDecl] = List.empty
      for(c <- list) {
        if(c != toRemove)
          toReturn = c :: toReturn
      }
      toReturn.reverse
    }

    def addClassVars(id: String, vars: List[VarDecl]) = {
      global.lookupClass(id) match {
        case None => // do nothing
        case Some(c) =>
          for(v <- vars) {
            v match {
              case VarDecl(tpe: Identifier,_) =>
                checkType(tpe)
              case _ => // either Bool, Int, Int[], String
            }
            v match {
              case VarDecl(tpe, ident: Identifier) =>
                ident match {
                  case Identifier(id) =>
                    c.parent match {
                      case None => // No problem, just continue
                      case Some(par) =>
                        //p.lookupVar(id) match {
                        checkForInheritance(par, id) match {
                          case Some(s) =>
                            error(s.name+" member declaration overrides previous declaration at "+s.position+".", ident)
                          case None => // No problem, just continue
                        }
                    }
                    c.lookupVar(id) match {
                      case Some(s) =>
                        error(id+" is declared more than once. First definition here: "+s.position+".", ident)
                      case None =>
                        v.setSymbol(new VariableSymbol(id).setPos(ident.file, Position.encode(ident.line, ident.col)))
                        tpe match {
                          case t: StringType => v.getSymbol.setType(TString)
                          case t: BooleanType => v.getSymbol.setType(TBoolean)
                          case t: IntType => v.getSymbol.setType(TInt)
                          case t: IntArrayType => v.getSymbol.setType(TIntArray)
                          case t: Identifier => v.getSymbol.setType(tpe.getType)
                        }
                        v.getSymbol match {
                          case s: VariableSymbol =>
                            c.members = c.members + (id -> s)
                            ident.setSymbol(s)
                            ident.setType(v.getSymbol.getType)
                            while(declaredVars.length <= s.id) {
                              // these lists should shadow each other
                              declaredVars = declaredVars :+ None
                              usedVars = usedVars :+ false
                            }
                           declaredVars(s.id) = Option(ident)
                          case _ => ??? // THIS CANNOT HAPPEN!!!!!!!!!!!(?)
                        }
                    }
                }
            }
          }
      }
    }

    def lookForInheritance(c: ClassSymbol, id: String, varOrMeth: String): Option[Symbol] = {
      c.parent match {
        case Some(p) =>
          varOrMeth match {
            case "var" =>
              p.lookupVar(id) match {
                case Some(s) => Option(s)
                case None => lookForInheritance(p, id, varOrMeth)
              }
            case "method" =>
              p.lookupMethod(id) match {
                case Some(s) => Option(s)
                case None => lookForInheritance(p, id, varOrMeth)

              }
          }
        case None => None
      }
    }

    def lookForMethodInheritance(m: MethodSymbol, id: String): Option[VariableSymbol] = {
      m.overridden match {
        case None => None
        case Some(s) =>
          s.lookupVar(id) match {
            case Some(v) => Option(v)
            case None => lookForMethodInheritance(s, id)
          }
      }
    }

    def addClassMethodDefsArgsAndVals(id: String, meths: List[MethodDecl]) = {
      global.lookupClass(id) match {
        case None => ??? // not possible? Class not added yet?
        case Some(c) =>
          for(m <- meths) {
            m match {
              case MethodDecl(retTyp, ident, _, _, _, _) =>
                retTyp match {
                  case t: StringType => retTyp.setType(TString)
                  case t: BooleanType => retTyp.setType(TBoolean)
                  case t: IntType => retTyp.setType(TInt)
                  case t: IntArrayType => retTyp.setType(TIntArray)
                  case id: Identifier =>
                    global.lookupClass(id.value) match {
                      case None => error("Undeclared Type: "+id.value+".")
                      case Some(cSym) =>
                        id.setSymbol(cSym)
                        id.setType(cSym.getType)
                    }
                }

                ident match {
                  case Identifier(id) =>
                    c.lookupMethod(id) match {
                      case Some(e) =>
                        error(id+" is defined more than once. First definition here: "+e.position+".", m)
                        m.setSymbol(new MethodSymbol(id, new ClassSymbol("error")))
                      case None =>
                        lookForInheritance(c, id, "method") match {
                          case Some(s: MethodSymbol) =>
                            m.setSymbol(new MethodSymbol(id, c).setPos(m.file, Position.encode(m.line, m.col)))
                            m.getSymbol match {
                              case sym: MethodSymbol =>
                                c.methods = c.methods + (id -> sym)
                                ident.setSymbol(sym)
                                sym.overridden = Option(s)
                              case _ => ??? // THIS CANNOT HAPPEN!!!!!!!!!!!(?)
                            }
                          case Some(s) => ??? // catches VariableSymbol and ClassSymbol
                          case None =>
                            m.setSymbol(new MethodSymbol(id, c).setPos(m.file, Position.encode(m.line, m.col)))
                            m.getSymbol match {
                              case sym: MethodSymbol =>
                                c.methods = c.methods + (id -> sym)
                                ident.setSymbol(sym)
                              case _ => ??? // THIS CANNOT HAPPEN!!!!!!!!!!!(?)
                            }
                        }
                    }
                }
            }
            m match {
              case MethodDecl(_,_,args,vars,_,_) =>
                for(f <- args) addFormal(m.getSymbol, f)
                // argList is built backwards
                m.getSymbol.argList = m.getSymbol.argList.reverse
                for(v <- vars) addMethodVars(m.getSymbol, v)
            }
          }
      }
    }
    def addClassMethodStats(id: String, meths: List[MethodDecl]) = {
      global.lookupClass(id) match {
        case None => ??? // not possible? Class not added yet?
        case Some(c) =>
          for(m <- meths) {
            m match {
              case MethodDecl(_,_,_,_,stats,retExpr) =>
                for(s <- stats) {
                  handleStatement(m.getSymbol, s)
                }
                if(m.hasSymbol)
                  handleExpression(m.getSymbol, retExpr)
            }
          }
      }
    }

    def addFormal(m: MethodSymbol, f: Formal) = {
      f match {
        case Formal(tpe, ident) =>
          checkType(tpe)
          ident match {
            case Identifier(id) =>
              m.lookupVar(id) match {
                case Some(_) =>
                  error("Parameter name "+id+" is used twice in "+m.name+".", ident)
                case None =>
                  f.setSymbol(new VariableSymbol(id).setPos(ident.file, Position.encode(ident.line, ident.col)))
                  tpe match {
                    case StringType() => f.getSymbol.setType(TString)
                    case BooleanType() => f.getSymbol.setType(TBoolean)
                    case IntType() => f.getSymbol.setType(TInt)
                    case IntArrayType() => f.getSymbol.setType(TIntArray)
                    case Identifier(_) => f.getSymbol.setType(tpe.getType)
                  }
                  f.getSymbol match {
                    case sym: VariableSymbol =>
                      m.params = m.params + (id -> sym)
                      m.argList = sym :: m.argList
                      ident.setSymbol(sym)
                      ident.setType(sym.getType)
                      while(declaredVars.length <= sym.id) {
                        declaredVars = declaredVars :+ None
                        usedVars = usedVars :+ false
                      }
                    case _ => ??? // THIS CANNOT HAPPEN!!!!!!!!!!!(?)
                  }
              }
          }

      }
    }

    def checkType(tpe: TypeTree): Unit = {
      tpe match {
        case ident: Identifier =>
          ident match {
            case Identifier(id) =>
              global.lookupClass(id) match {
                case Some(s) => ident.setSymbol(s); ident.setType(tpe.getType)
                case None =>
                  fatal("Undeclared type: "+id+".", ident)

              }
          }

        case _ => // either Bool, Int, Int[], String
      }
    }

    def addMethodVars(m: MethodSymbol, v: VarDecl) = {
      v match {
        case VarDecl(tpe,ident) =>
          checkType(tpe)
          ident match {
            case Identifier(id) =>
              m.lookupVar(id) match {
                case Some(s) =>
                  if(m.argList.contains(s))
                    error("Declaration of "+id+" as local shadows method parameter of the same name.", ident)
                  else
                    error( id+" is declared more than once. First declaration here: "+s.position+".", ident)
                case None =>
                  v.setSymbol(new VariableSymbol(id).setPos(ident.file, Position.encode(ident.line, ident.col)))
                  tpe match {
                    case StringType() => v.getSymbol.setType(TString)
                    case BooleanType() => v.getSymbol.setType(TBoolean)
                    case IntType() => v.getSymbol.setType(TInt)
                    case IntArrayType() => v.getSymbol.setType(TIntArray)
                    case Identifier(_) => v.getSymbol.setType(tpe.getType)
                  }
                  v.getSymbol match {
                    case s: VariableSymbol =>
                      m.members = m.members + (id -> s)
                      ident.setSymbol(s)
                      ident.setType(s.getType)
                      while(declaredVars.length <= s.id) {
                        // these lists should shadow each other
                        declaredVars = declaredVars :+ None
                        usedVars = usedVars :+ false
                      }
                      declaredVars(s.id) = Option(ident)
                    case _ => ??? // THIS CANNOT HAPPEN!!!!!!!!!!!(?)
                  }
              }
          }
        case _ => // either Bool, Int, Int[], String
      }
    }

    def handleStatement(sym: Symbol, stat: StatTree): Unit = {
      currentPos = Position.encode(stat.line, stat.col)
      stat match {
        case Block(stats) => for(s <- stats) handleStatement(sym, s)

        case If(expr, thn, els) =>
          handleExpression(sym, expr)
          handleStatement(sym, thn)
          els match {
            case Some(s) => handleStatement(sym, s)
            case None =>
          }

        case While(expr, stat) =>
          handleExpression(sym, expr)
          handleStatement(sym, stat)

        case Println(expr) =>
          handleExpression(sym, expr)

        case Assign(ident, expr) =>
          handleExpression(sym, ident)
          handleExpression(sym, expr)

        case ArrayAssign(ident, index, expr) =>
          handleExpression(sym, ident)
          handleExpression(sym, index)
          handleExpression(sym, expr)
      }
    }

    def handleExpression(sym: Symbol, expr: ExprTree): Unit = {
      expr match {
        case And(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case Or(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case Plus(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case Minus(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case Times(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case Div(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case Mod(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case LessThan(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case Equals(lhs, rhs) => handleExpression(sym, lhs); handleExpression(sym, rhs)
        case ArrayRead(arr, index) => handleExpression(sym, arr); handleExpression(sym, index)
        case ArrayLength(arr) => handleExpression(sym, arr)
        case MethodCall(obj, meth, args) =>
          obj match {
            case t: This => handleExpression(sym, t)
            case ident: Identifier => handleExpression(sym, ident)
            case New(ident: Identifier) =>
              ident match {
                case Identifier(id) =>
                  global.lookupClass(id) match {
                    case Some(s) => ident.setSymbol(s); ident.setType(s.getType)
                    case None =>
                      error("Undeclared type: "+id+".", ident)
                  }
              }
            case m: MethodCall =>
              // handle the call recursively, eventually we will find a methodcall with obj = Identifier(id)
              // that call will tag the Identifier so no need to do anything more here
              handleExpression(sym, m)
            case _ => // Nothing to do here - flies away
          }

          for(e <- args) handleExpression(sym, e)

        case ident: Identifier =>
          ident match {
            case Identifier(id) =>
              sym match {
                case c: ClassSymbol =>
                  c.lookupVar(id) match {
                    case Some(s) =>
                      ident.setSymbol(s)
                      ident.setType(s.getType)
                      usedVars(s.id) = true
                    case None =>
                      error("Undeclared variable: "+id+".", ident)
                  }
                case m: MethodSymbol =>
                  // is ident a variable declared in the method or argument to method?
                  m.lookupVar(id) match {
                    case Some(s) =>
                      ident.setSymbol(s)
                      ident.setType(s.getType)
                      //if(s.id < usedVars.length)
                        usedVars(s.id) = true
                      // in rare cases: id > length, e.g. if args are used at the end
                    case None => // is it declared in the class?
                      m.classSymbol.lookupVar(id) match {
                        case Some(cMember) =>
                          ident.setSymbol(cMember)
                          ident.setType(cMember.getType)
                          usedVars(cMember.id) = true
                        case None =>
                          // is it inherited from parent class?
                          m.classSymbol.parent match {
                            case None => // unknown variable, no parent
                              error("Undeclared identifier: "+id+".", ident)
                            case Some(p) =>
                              checkForInheritance(p, id) match {
                                case None => // unknown variable, not in any ancestor
                                  error("Undeclared identifier: "+id+".", ident)
                                case Some(pMember) =>
                                  ident.setSymbol(pMember)
                                  ident.setType(pMember.getType)
                                  usedVars(pMember.id) = true
                              }
                          }
                      }
                  }
                case _ => ???
              }
          }
        case t: This =>
          sym match {
            case c: ClassSymbol => t.setSymbol(c); t.setType(c.getType)
            case m: MethodSymbol => t.setSymbol(m.classSymbol); t.setType(m.classSymbol.getType)
            case _ => ??? // should not happen
          }
        case NewIntArray(expr) => handleExpression(sym, expr)
        case New(ident) =>
          ident match {
            case Identifier(typeID) =>
              global.lookupClass(typeID) match {
                case Some(s) =>
                  ident.setSymbol(s)
                  ident.setType(s.getType)
                case None => ??? // error unknown class
              }
          }
        case Not(expr) => handleExpression(sym, expr)
        case _ => // do nothing
      }
    }

    def checkForInheritance(p: ClassSymbol, id: String): Option[VariableSymbol] = {
      // Look for inheritance recursively
      p.lookupVar(id) match {
        case Some(sym) => Option(sym)
        case None =>
          // Do we have more generations where variable can be found?
          p.parent match {
            case None => None // No more parents, i.e. variable not found
            case Some(par) => checkForInheritance(par, id) // Check next generation
          }
      }
    }

    def checkForCycle(c: ClassDecl, ref: String): Option[String] = {
      c match {
        case ClassDecl(Identifier(id), parent, _, _) =>
          if(id == ref) // base case if cycle
            Option(" <: " + id)
          else {
            parent match {
              case Some(Identifier(pid)) =>
                getClassByID(pid, classList) match {
                  case Some(p) =>
                    checkForCycle(p, ref) match {
                      case Some(str) => Option(" <: "+ id + str)
                      case None => None
                    }
                  case None => ??? // Inheritence of unknown class, ERROR!!!
                }
              case None => None // base case if no cycle
            }
          }
      }
    }
   def addTheRest(classes: List[ClassDecl]): Unit = {
     if (!classes.isEmpty && !processed.contains(classes.head)) { // all classes haven't been processed yet
       val c = classes.head
       c match {
         case ClassDecl(Identifier(id), parent, vars, methods) =>
           parent match {
             case None => // Class dosen't extend another class
             case Some(par) => // Class extends
               par.getSymbol match {
                 case cs: ClassSymbol => getClassBySym(cs, classList) match {
                   // find parent
                   case Some(p) =>
                     checkForCycle(p, id) match {
                       case None => // No problem, not a cycle
                       case Some(cycle) => fatal("Cyclic inheritance graph: " + id + cycle)
                     }
                     if (!processed.contains(p)) {
                       // then process parent
                       addTheRest(p :: List.empty)
                       processed = p :: processed

                     } // otherwise, continue
                   case None => // ERROR inheritance of unknown class
                 }
                 case _ => ??? // the parent must have a class symbol
               }
           } // parent is by now added, now add the class
           addClassVars(id, vars)
           addClassMethodDefsArgsAndVals(id, methods)
           processed = c :: processed
           addTheRest(classes.tail)
       }
     }
   }

    prog match {
      case Program(main,classes) =>
        classList = classes
        main match {
          case MainObject(ident: Identifier, _) =>
            ident match {
              case Identifier(id) =>
                main.setSymbol(new ClassSymbol(id).setPos(ident.file, Position.encode(ident.line, ident.col))) // main.getSymbol.
                main.getSymbol.setType(new TObject(main.getSymbol))
                global.classes = global.classes + (id -> main.getSymbol)
                ident.setSymbol(main.getSymbol)
                ident.setType(main.getSymbol.getType)
            }

        }
        for(c <- classes) addClass(c)
        processed = List.empty // reset this list before processing vars and meths
        addTheRest(classes)
        main match {
          case MainObject(_, stats) =>
            for(s <- stats) handleStatement(main.getSymbol, s)
        }
        for(ClassDecl(Identifier(id),_,_,meths) <- classes) addClassMethodStats(id, meths)
      case _ => ???
    }

    //println("DONE WITH NAMEANALYSIS!")
    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    terminateIfErrors

    // Check for unused variables
    for(i <- 1 until usedVars.length) {
      if(!usedVars(i)) { // potentially not used variable
        declaredVars(i) match {
          case None => // ID not associated with a variable
          case Some(ident: Identifier) =>
            ident match {
              case Identifier (id) =>
                warning ("Variable " + id + " is declared but never used.", ident)
            }
        }
      }
    }

    prog

  }


}
