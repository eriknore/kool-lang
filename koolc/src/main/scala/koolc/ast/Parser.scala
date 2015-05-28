package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)
    //var pos: Int = 0
    var file: java.io.File = null
    
    val extractID = """ID\((.*)\)""".r
    val extractSTR = """STR\((.*)\)""".r
    val extractINT = """INT\((.*)\)""".r
    val extractINTLIT = """IntLit\((.*)\)""".r
     
    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {
      val pos = currentPos
      val main = mainObject
      eat(RBRACE)
      var classes: List[ClassDecl] = List.empty
      currentToken.kind match {
        case CLASS =>
          while(currentToken.kind != EOF) {
              classes = classDecl :: classes
              eat(RBRACE)
          }
        case _ =>
      }
      eat(EOF)
      new Program(main, classes.reverse).setPos(file, pos)
    }
    
    def mainObject: MainObject = {
      val pos = currentPos
      var statementList: List[StatTree] = List.empty
      eat(OBJECT) 
      // first token read so we can set global variable file 
      // to the file being read
      file = currentToken.file
      val mainID = identifier
      eat(LBRACE)
      eat(DEF)
      eat(MAIN); eat(LPAREN); eat(RPAREN)
      eat(COLON)
      eat(UNIT)
      eat(EQSIGN)
      eat(LBRACE)
      while(!isEndOfBlock) {
        statementList = statement :: statementList
      }
      eat(RBRACE)
      new MainObject(mainID, statementList.reverse).setPos(file, pos)
    }
    
    def classDecl: ClassDecl = {
      val pos = currentPos
      eat(CLASS)
      val id = identifier
      var parent: Option[Identifier] = None
      var vars: List[VarDecl] = List.empty
      var methods: List[MethodDecl] = List.empty
      currentToken.kind match {
        case EXTENDS => eat(EXTENDS); parent = Some(identifier)
        case LBRACE => 
        case _ => expected(EXTENDS, LBRACE)
      }
      eat(LBRACE)
      while(currentToken.kind == VAR) {
        vars = varDecl :: vars;
      }
      while(currentToken.kind == DEF) {
        methods = methodDecl :: methods;
      }
      new ClassDecl(id, parent, vars.reverse, methods.reverse).setPos(file, pos)
    }
    
    def varDecl: VarDecl = {
      val pos = currentPos
      eat(VAR)
      val id = identifier
      eat(COLON)
      val tpe = typeDecl
      eat(SEMICOLON)
      new VarDecl(tpe, id).setPos(file, pos)
    }
    
    def typeDecl: TypeTree = {
      val pos = currentPos
      currentToken.kind match {
        case BOOLEAN => eat(BOOLEAN); new BooleanType().setPos(file, pos)
        case STRING => eat(STRING); new StringType().setPos(file, pos)
        case INT =>
          eat(INT)
          currentToken.kind match {
            case LBRACKET => eat(LBRACKET); eat(RBRACKET); new IntArrayType().setPos(file, pos)
            case _ => new IntType().setPos(file, pos)
          }
        case _ => identifier
      }
    }
    
    def methodDecl: MethodDecl = {
      val pos = currentPos
      var args: List[Formal] = List.empty
      var vars: List[VarDecl] = List.empty
      var stats: List[StatTree] = List.empty
      eat(DEF)
      val id = identifier
      eat(LPAREN)
      if(currentToken.kind != RPAREN)
        args = formal :: args
      while(currentToken.kind != RPAREN) {
        eat(COMMA)
        args = formal :: args
      }
      eat(RPAREN)
      eat(COLON)
      val retType = typeDecl
      eat(EQSIGN)
      eat(LBRACE)
      while(currentToken.kind == VAR)
        vars = varDecl :: vars
      while(currentToken.kind != RETURN)
        stats = statement :: stats
      eat(RETURN)
      val retExpr = expression
      eat(SEMICOLON)
      eat(RBRACE)
      new MethodDecl(retType, id, args.reverse, vars.reverse, stats.reverse, retExpr).setPos(file, pos)
    }
    
    def formal: Formal = {
      val pos = currentPos
      val id = identifier
      eat(COLON)
      val tpe = typeDecl
      new Formal(tpe, id).setPos(file, pos)
    }
    
    def statement: StatTree = {
      
      currentToken.kind match {
        
        case LBRACE => // Block
          val pos = currentPos
          eat(LBRACE)
          var statementList: List[StatTree] = List.empty
          while(!isEndOfBlock) {
            statementList = statement :: statementList
          }
          eat(RBRACE)
          new Block(statementList.reverse).setPos(file, pos)
        
        case IF =>
          val pos = currentPos
          eat(IF)
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          var ifStat = statement
          // Let's return this if we don't encounter an else
          val noElse = new If(expr, ifStat, None).setPos(file, pos)
          if (currentToken.kind == ELSE) {
            eat(ELSE)
            new If(expr, ifStat, Some(statement)).setPos(file, pos)
          } else {
            noElse
          }
        
        case WHILE =>
          val pos = currentPos
          eat(WHILE)
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          new While(expr, statement).setPos(file, pos)
        
        case PRINTLN =>
          val pos = currentPos
          eat(PRINTLN)
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          eat(SEMICOLON)
          new Println(expr).setPos(file, pos)
        
        case IDKIND => // Assign or ArrayAssign
          val pos = currentPos
          val id = identifier
          currentToken.kind match {
            
            case EQSIGN   => 
              eat(EQSIGN)
              val assign = new Assign(id, expression).setPos(file, pos)
              eat(SEMICOLON)
              assign
            
            case LBRACKET =>
              eat(LBRACKET)
              val lhs = expression
              eat(RBRACKET)
              eat(EQSIGN)
              val rhs = expression
              eat(SEMICOLON)
              new ArrayAssign(id, lhs, rhs).setPos(file, pos)
            
            case _ => expected(EQSIGN, LBRACKET)
          }
        
        case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
      }
    }
    
    def expression: ExprTree = {
      follow(atom)
    }
    
    def newArrayOrClass: ExprTree = {
      val pos = currentPos
      eat(NEW)
      currentToken.kind match {
        case INT => 
          eat(INT)
          eat(LBRACKET)
          val expr = expression
          eat(RBRACKET)
          new NewIntArray(expr).setPos(file, pos)
        case IDKIND =>
          val id = identifier
          eat(LPAREN)
          eat(RPAREN)
          currentToken.kind match {
            case DOT => dotLengthOrMCall(New(id).setPos(file, pos), pos)
            case _ => New(id).setPos(file, pos)
          }

        case _ => expected(INT, IDKIND)
      }
    }
    
    def follow(expr: ExprTree): ExprTree = {
      val pos = currentPos
      if(!currentIsFollower) {
        expr
      } else {
        currentToken.kind match {
          case LBRACKET => arrayRead(expr, pos)
          case DOT => dotLengthOrMCall(expr, pos)
          case _ => follow(or(expr))
        }
      }
    }
    
    def or(expr: ExprTree): ExprTree = {
      val pos = currentPos
      val lhs = and(expr)
      currentToken.kind match {
        case OR => eat(OR); Or(lhs, and(atom)).setPos(file, pos)
        case _ => lhs
      }
    }
    
    def and(expr: ExprTree): ExprTree = {
      val pos = currentPos
      val lhs = comp(expr)
      currentToken.kind match {
        case AND => eat(AND); And(lhs, comp(atom)).setPos(file, pos)
        case _ => lhs
      }
    }
    
    def comp(expr: ExprTree): ExprTree = {
      val pos = currentPos
      val lhs = term(expr)
      currentToken.kind match {
        case EQUALS => eat(EQUALS); ; Equals(lhs, term(atom)).setPos(file, pos)
        case LESSTHAN => eat(LESSTHAN); LessThan(lhs, term(atom)).setPos(file, pos)
        case _ => lhs
      }
    }
    
    def term(expr: ExprTree): ExprTree = {
      val pos = currentPos
      val l = factor(expr)
      currentToken.kind match {
        case PLUS => eat(PLUS); Plus(l, factor(atom)).setPos(file, pos)
        case MINUS => eat(MINUS); Minus(l, factor(atom)).setPos(file, pos)
        case _ => l
      }
    }
    
    def factor(expr: ExprTree): ExprTree = {
      val pos = currentPos
      currentToken.kind match {
        case TIMES => eat(TIMES); Times(expr, atom).setPos(file, pos)
        case MOD => eat(MOD); Mod(expr, atom).setPos(file, pos)
        case DIV => eat(DIV); Div(expr, atom).setPos(file, pos)
        case _ => expr
      }
    }
    
    def atom: ExprTree = {
      val pos = currentPos
      currentToken.kind match {
        case INTLITKIND => closeAtom(intLit)
        case STRLITKIND => closeAtom(strLit)
        case IDKIND => closeAtom(identifier)
        case THIS =>  eat(THIS); closeAtom(new This().setPos(file, pos))
        case TRUE => eat(TRUE); closeAtom(new True().setPos(file, pos))
        case FALSE => eat(FALSE); closeAtom(new False().setPos(file, pos))
        case BANG => eat(BANG); new Not(atom)
        case NEW => newArrayOrClass
        case LPAREN => 
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          expr
        case _ => expected(TRUE, FALSE, THIS, INTLITKIND, STRLITKIND, LPAREN, IDKIND, NEW, BANG)
      }
    }
    
    def closeAtom(atom: ExprTree): ExprTree = {
      val pos = currentPos
      currentToken.kind match {
        case LBRACKET => arrayRead(atom, pos)
        case DOT => dotLengthOrMCall(atom, pos)
        case _ => atom
      }
    }
    
    def arrayRead(id: ExprTree, pos: Int): ExprTree = {
      eat(LBRACKET); 
      val index = expression
      eat(RBRACKET)
      new ArrayRead(id, index).setPos(file, pos)
    }
    
    def dotLengthOrMCall(id: ExprTree, pos: Int): ExprTree = {
      eat(DOT)
      currentToken.kind match {
        case LENGTH => eat(LENGTH); closeAtom(new ArrayLength(id).setPos(file, pos))
        case IDKIND => // Method call
          val method = identifier
          var args: List[ExprTree]= List.empty
          eat(LPAREN)
          currentToken.kind match {
            case RPAREN =>
            case _ =>
              val e = expression 
              args = e :: args
              while(currentToken.kind != RPAREN) {
                eat(COMMA)
                args = expression :: args
              }
          }
          eat(RPAREN)
          closeAtom(new MethodCall(id, method, args.reverse).setPos(file, pos+1))
        case _ => expected(LENGTH, IDKIND)
      }
    }
    
    def strLit: ExprTree = {
      val pos = currentPos
      val str = currentToken.toString() match {
        case extractSTR(str) => str
        case _ => expected(STRLITKIND)
      }
      eat(STRLITKIND)
      new StringLit(str).setPos(file, pos)
    }
    
    def intLit: ExprTree = {
      val pos = currentPos
      val i = currentToken.toString() match {
        case extractINT(i) => i
        case _ => expected(INTLITKIND)
      }
      eat(INTLITKIND)
      try {
        new IntLit(i.toInt).setPos(file, pos)
      } catch {
        case e: NumberFormatException => new IntLit(-1) // good handle?
      }
    }
    
    def identifier: Identifier = {
      val pos = currentPos
      val id = currentToken.toString match {
        case extractID(id) => id
        case _ => expected(IDKIND)
      }
      eat(IDKIND)
      new Identifier(id).setPos(file, pos)
    }
        
    def isEndOfBlock = currentToken.kind == RBRACE
    def currentIsFollower = {
      val c = currentToken.kind
      (c == AND || c == OR || c == EQUALS || c == LESSTHAN || c == PLUS
          || c == MINUS || c == TIMES || c == MOD || c == DIV || c == LBRACKET || c == DOT)
    }
    
    def currentPos = Position.encode(currentToken.line, currentToken.col)
    
    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
