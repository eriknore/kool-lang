package koolc.utils

import java.io.File

import koolc.ast.Trees._

import scala.io.Source
import scala.sys.process._ // to call bash

object AstConverter extends Pipeline[File, Program] {

  class Node(id: String, row: Int, col: Int) {
    def getPos: Int = Position.encode(row, col)
    def getId: String = id
    def getInt: Int = id.toInt
    def getBool: Boolean = id.toBoolean
  }

  def run(ctx: Context)(f: File): Program = {
    import ctx.reporter._


    val filename = f.getAbsolutePath
    val sb = new StringBuilder

    try {
      val status = "./kool-parser " + filename ! ProcessLogger(line => sb.append(line + '\n'))
      if (status != 0)
        fatal("Error: " + sb) // error message is written to stdout as well, not stderr
    } catch {
      case e: java.io.IOException => fatal(
        """The file 'kool-parser' was not found, it is required when running with bnfc-flag.
          |It can be generated using the supplied Makefile (see README-BNFC-extension for more info). Make sure that
          |the following programs are installed before running 'make':
          |    bnfc
          |    gmp
          |    ghc – haskell output
          |    alex – haskell output
          |    happy – haskell output
          |""".stripMargin)
    }

    val split = sb.toString.split("\n")
    assert(split(2) == "Parse Successful!")

    val source = Source.fromString(split(6))
    var current = ' '
    def readNext: Char = {
      if(source.hasNext) current = source.next
      current
    }

    def ensureNextWordIs(str: String): Unit = {
      val temp = readWord
      if(str != temp)
        fatal("Expected " + str + ", got " + temp + " before\n" + flushSource)
    }

    def ensureNextIs(c: Char): Unit = {
      val temp = readNext
      if(c != temp)
        fatal("Expected '" + c + "', got '" + temp + "' before\n" + flushSource)
    }

    def flushSource: String = {
      val sb = StringBuilder.newBuilder
      while(source.hasNext)
        sb.append(source.next)
      sb.toString
    }

    def parseTree: Program = {
      ensureNextWordIs("Program")
      Program(readMain, readClasses)
    }

    def readMain: MainObject = {
      ensureNextIs('(')
      ensureNextWordIs("MainObject")
      val main = extractNode
      ensureNextIs(' ')
      val id = extractNode
      ensureNextIs(' ')
      extractNode // PosDef: is read away because not needed here, only in MethodDecl
      ensureNextIs(' ')
      val stmnts = readStmnts
      ensureNextIs(')')
      ensureNextIs(' ')
      MainObject(new Identifier(id.getId)setPos(f, id.getPos), stmnts).setPos(f, main.getPos)
    }

    def readClasses: List[ClassDecl] = {
      ensureNextIs('[')
      var list: List[ClassDecl] = List.empty
      while(current != ']') {
        readClass match {
          case None => // current = ']'
          case Some(c) =>
            list = c :: list
            readNext
        }
      }
      list.reverse
    }

    def readClass: Option[ClassDecl] = {
      if(readNext != ']') {
        ensureNextWordIs("Decl") // 'C' has been read :p
        val pos = extractNode
        ensureNextIs(' ')
        val ident = readIdent
        ensureNextIs(' ')
        val ext = readExtends
        ensureNextIs(' ')
        val vars = readVars
        ensureNextIs(' ')
        val meths = readMeths
        Option(ClassDecl(ident, ext, vars, meths).setPos(f, pos.getPos))
      }
      else None
    }

    def readExtends: Option[Identifier] = {
      /* If we have an extends then next char is '(', otherwise next char is 'E'
         as in "EEmp" */
      if(readNext == '(') { // with extends
        ensureNextWordIs("EExt")
        val stmnt = readIdent
        ensureNextIs(')')
        Option(stmnt)
      } else { // no extends, 'E' already read!
        // if we run ensureNextWordIs("Emp"), we read away ' ' which is needed after returning
        ensureNextIs('E')
        ensureNextIs('m')
        ensureNextIs('p')
        None
      }
    }

    def readVars: List[VarDecl] = {
      ensureNextIs('[')
      var list: List[VarDecl] = List.empty
      while(current != ']') {
        readVar match {
          case None => // current = ']'
          case Some(v) =>
            list = v :: list
            if(current != ']')
              readNext // ',' or ']'
        }
      }
      list.reverse
    }

    def readVar: Option[VarDecl] = {
      if(readNext != ']') {
        ensureNextWordIs("Decl") // 'V' has been read
        val pos = extractNode
        ensureNextIs(' ')
        val ident = readIdent
        ensureNextIs(' ')
        val tpe = readType
        Option(VarDecl(tpe, ident).setPos(f, pos.getPos))
      } else
        None
    }

    def readType: TypeTree = {
      ensureNextIs('(')
      val tpe = readWord match {
        case "TIntArray" => extractNode; IntArrayType()
        case "TInt" => extractNode; IntType()
        case "TBoolean" => extractNode; BooleanType()
        case "TString" => extractNode; StringType()
        case "TIdent" => readIdent
      }
      ensureNextIs(')')
      tpe
    }

    def readMeths: List[MethodDecl] = {
      ensureNextIs('[')
      var list: List[MethodDecl] = List.empty
      while(current != ']') {
        readMeth match {
          case None => // current = ']'
          case Some(m) =>
            list = m :: list
            if(current != ']')
              readNext // ',' or ']'
        }
      }
      list.reverse
    }

    def readMeth: Option[MethodDecl] = {
      if(readNext != ']') {
        val word = readWord // 'M' has been read: Decl
        val pos = extractNode
        ensureNextIs(' ')
        val ident = readIdent
        ensureNextIs(' ')
        val args = readFormals
        ensureNextIs(' ')
        val tpe = readType
        ensureNextIs(' ')
        val vars = readVars
        ensureNextIs(' ')
        val stmnts = readStmnts
        ensureNextIs(' ')
        val retExpr = readExprWithParen
        Option(MethodDecl(tpe, ident, args, vars, stmnts, retExpr).setPos(f, pos.getPos))
      }
      else None
    }

    def readFormals: List[Formal] = {
      var list: List[Formal] = List.empty
      ensureNextIs('[')
      while(current != ']') {
        readFormal match {
          case None => // current = ']'
          case Some(f) =>
            list = f :: list
            if(current != ']')
              readNext // ',' or ']'
        }
      }
      list.reverse
    }

    def readFormal: Option[Formal] = {
      if(readNext != ']') {
        ensureNextWordIs("Args") // 'M' has been read :p
        val ident = readIdent
        ensureNextIs(' ')
        val tpe = readType
        val pos = Position.encode(ident.line, ident.col)
        Option(Formal(tpe, ident).setPos(f, pos))
      }
      else None
    }

    def readStmnts: List[StatTree] = {
      var list: List[StatTree] = List.empty
      ensureNextIs('[')
      while(current != ']') {
        readStmnt match {
          case None => // current = ']'
          case Some(s) =>
            list = s :: list
            readNext
        }
      }
      list.reverse
    }

    def readStmntWithParen: StatTree = {
      ensureNextIs('(')
      val stmnt = readStmnt
      ensureNextIs(')')
      stmnt match {
        case Some(s) => s
        case None => fatal("Expected Statement.\nRest of file:\n"+flushSource)
      }
    }

    def readStmnt: Option[StatTree] = {
      if(readNext != ']') { // reads away 'S'
      val stmnt = readWord match {
          case "Block" => Block(readStmnts)
          case "IfEmp" =>
            val pos = extractNode
            ensureNextIs(' ')
            val expr = readExprWithParen
            ensureNextIs(' ')
            val stmnt = readStmntWithParen
            If(expr, stmnt, None).setPos(f, pos.getPos)
          case "IfElse" =>
            val pos = extractNode
            ensureNextIs(' ')
            val expr = readExprWithParen
            ensureNextIs(' ')
            val stmnt = readStmntWithParen
            ensureNextIs(' ')
            val els = readStmntWithParen
            If(expr, stmnt, Option(els)).setPos(f, pos.getPos)
          case "While" =>
            val w = extractNode
            ensureNextIs(' ')
            val expr = readExprWithParen
            ensureNextIs(' ')
            While(expr, readStmntWithParen).setPos(f, w.getPos)
          case "Println" =>
            val pr = extractNode
            ensureNextIs(' ')
            val expr = readExprWithParen
            Println(expr).setPos(f, pr.getPos)
          case "Assign" =>
            val assignee = readIdent
            ensureNextIs(' ')
            val expr = readExprWithParen
            // Assign should have same pos as assignee
            val pos = Position.encode(assignee.line, assignee.col)
            Assign(assignee, expr).setPos(f, pos)
          case "ArrayAssign" =>
            val id = readIdent
            ensureNextIs(' ')
            val index = readExprWithParen
            ensureNextIs(' ')
            val expr = readExprWithParen
            val pos = Position.encode(id.line, id.col)
            ArrayAssign(id, index, expr).setPos(f, pos)
          case error => fatal("Stmnt not recognized: '" + error + "'\nRest of file:\n" + flushSource)
        }
        Option(stmnt)
      }
      else None
    }

    def readArgs: List[ExprTree] = {
      var list: List[ExprTree] = List.empty
      ensureNextIs('[')
      // now next is either ] as in []
      //                      or E as in EPlus
      readNext
      while(current != ']') {
        list = readExpr :: list
        readNext // is either ',' or ']'
        if(current != ']')
          ensureNextIs('E') // read away, E as in Expression
      }
      list.reverse
    }

    def readExprWithParen: ExprTree = {
      ensureNextIs('(')
      ensureNextIs('E') // read away, E as in Expression
      val expr = readExpr
      ensureNextIs(')')
      expr
    }

    def readExpr: ExprTree = {
      readWord match {
        case "MethodCall" =>
          val obj = readExprWithParen
          ensureNextIs(' ')
          val method = readIdent
          ensureNextIs(' ')
          val args = readArgs // next is either [] or [...]
        val pos = Position.encode(obj.line, obj.col)
          MethodCall(obj, method, args).setPos(f, pos)
        case "ArrayRead" =>
          val arr = readExprWithParen
          ensureNextIs(' ')
          val expr = readExprWithParen
          val pos = Position.encode(arr.line, arr.col)
          ArrayRead(arr, expr).setPos(f, pos)
        case "ArrayLength" =>
          val expr = readExprWithParen
          ArrayLength(expr).setPos(f, Position.encode(expr.line, expr.col))
        case "NewIntArray" =>
          val node = extractNode
          ensureNextIs(' ')
          NewIntArray(readExprWithParen).setPos(f, node.getPos)
        case "New" =>
          val n = extractNode // new
          ensureNextIs(' ')
          val obj = readIdent // classname()
          New(obj).setPos(f, n.getPos)
        case "Or" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val o = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          Or(lhs, rhs).setPos(f, o.getPos)
        case "And" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val a = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          And(lhs, rhs).setPos(f, a.getPos)
        case "Equals" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val e = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          Equals(lhs, rhs).setPos(f, e.getPos)
        case "LessThan" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val lt = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          LessThan(lhs, rhs).setPos(f, lt.getPos)
        case "Mod" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val t = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          Mod(lhs, rhs).setPos(f, t.getPos)
        case "Times" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val t = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          Times(lhs, rhs).setPos(f, t.getPos)
        case "Div" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val t = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          Div(lhs, rhs).setPos(f, t.getPos)
        case "Minus" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val m = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          Minus(lhs, rhs).setPos(f, m.getPos)
        case "Plus" =>
          val lhs = readExprWithParen
          ensureNextIs(' ')
          val p = extractNode
          ensureNextIs(' ')
          val rhs = readExprWithParen
          Plus(lhs, rhs).setPos(f, p.getPos)
        case "Not" =>
          val str = extractNode
          ensureNextIs(' ')
          val expr = readExprWithParen
          Not(expr).setPos(f, str.getPos)
        case "This" =>
          val str = extractNode
          This().setPos(f, str.getPos)
        case "BoolFalse" =>
          val str = extractNode
          False().setPos(f, str.getPos)
        case "BoolTrue" =>
          val str = extractNode
          True().setPos(f, str.getPos)
        case "Ident" => readIdent
        case "IntLit" =>
          val str = extractNode
          IntLit(str.getInt).setPos(f, str.getPos)
        case "StringLit" =>
          val str = extractNode
          StringLit(str.getId).setPos(f, str.getPos)
        case _ => fatal("Unrecognized expression error. Rest of file:\n"+flushSource)
      }
    }

    def readIdent: Identifier = {
      val id = extractNode
      Identifier(id.getId).setPos(f, id.getPos)
    }

    def readString: String = {
      val sb = StringBuilder.newBuilder
      ensureNextIs('"')
      sb.append(readNext)
      while(current != '"')
        sb.append(readNext)

      /* a StringLit from BNFC has the form "\"<String>\"", which means our read string
         has only read \" before ending - this is a special case handled below. Note that
         we need to escape special characters '\' and '"', i.e. "\\" and "\"" */
      if(sb.toString == "\\\"") {
        sb.clear()
        sb.append(readNext)
        while(current != '"')
          sb.append(readNext)

        /* Now we have in sb.toString -> <String>\", need to drop 2, and read away the
           last '"' */
        ensureNextIs('"')
        sb.dropRight(2).toString
      } else
        sb.dropRight(1).toString
    }

    def readInt: Int = {
      val sb = StringBuilder.newBuilder
      sb.append(readNext)
      while(current.isDigit)
        sb.append(readNext)
      sb.dropRight(1).toInt
    }

    def readWord: String = {
      val sb = StringBuilder.newBuilder
      sb.append(readNext)
      while(current.isLetter)
        sb.append(readNext)
      sb.toString.dropRight(1)
    }

    def extractNode: Node = {
      ensureNextIs('(')
      readWord // read away, e.g. PosClassIdent
      ensureNextIs('(')
      ensureNextIs('(')
      val row = readInt
      val col = readInt
      ensureNextIs(',')
      val id = readString
      ensureNextIs(')')
      ensureNextIs(')')
      new Node(id, row, col)
    }

    parseTree
  }
}
