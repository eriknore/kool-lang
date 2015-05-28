package koolc
package analyzer

import utils._
import Types._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
    
    def hasSymbol: Boolean = _sym match {
      case Some(_) => true
      case None => false
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String,ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String,MethodSymbol]()
    var members = Map[String,VariableSymbol]()

    // If we can't find a method in our method-map, then it is
    // probably in our parent's method-map.
    // Overriding is checked in NameAnalysis
    def lookupMethod(n: String): Option[MethodSymbol] = {
      methods.get(n) match {
        case Some(s) => Option(s)
        case None => None
      }
    }

    // Same story here
    def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) match {
        case Some(s) => Option(s)
        case None    => None
      }
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String,VariableSymbol]()
    var members = Map[String,VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden : Option[MethodSymbol] = None

    // Consider:
    // class foo {
    //   val i: Int               <---- i from class-members
    //   def func(val i: Int) = { <---- i from params
    //     val i: Int             <---- i from members
    //     print(i)               <---- print the i from params
    //   }
    // }
    // Det där är väl hierarkin? params är lite konstigt namn det
    // kanske inte stämmer, fast det beror på hur vi fyller i
    // params/members i NameAnalysis
    def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) match {
        case Some(s) => Option(s)
        case None    =>
          params.get(n) match {
            case Some(s) => Option(s)
            // the following uses the classSymbol lookup method, otherwise we could init
            // method-members to class-members
            case None => None
          }
      }
    }
  }

  class VariableSymbol(val name: String) extends Symbol

}
