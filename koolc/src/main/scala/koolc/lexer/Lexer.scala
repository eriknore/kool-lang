package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._
  
  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._
  
    new Iterator[Token] {
      
      var current = source.next
      var sentEOF = false
      var sb: StringBuilder = new StringBuilder
      
      def readNext = if(source.hasNext) current = source.next
      
      def hasNext = {
        !sentEOF
      }
      
      def next = {
        while(isWhitespace && source.hasNext)
          readNext
        if(!source.hasNext) {
          endOfFile
        } else {
          val position = source.pos
          if(current.isLetter) identifierOrKeyword
          else if(current == '"') strLit
          else if(current == '0') { readNext; new INTLIT(0).setPos(f, source.pos-1) }
          else if(current.isDigit) intLit
          else if (isSingleSpecialChar) {
            val temp = current; readNext
            specChar(temp.toString).setPos(f, position)} 
          else if(isDoubleSpecialChar) doubleSpecChar
          else fatal("Invalid character: "+current, new Token(BAD).setPos(f,source.pos))
        }
      }
  
      def doubleSpecChar = {
        val position = source.pos
        var previous = current
        readNext
        current match {
          case '&' if(previous == '&') => readNext; new Token(AND).setPos(f, position)
          case '|' if(previous == '|') => readNext; new Token(OR).setPos(f, position)
          case '=' if(previous == '=') => readNext; new Token(EQUALS).setPos(f, position)
          
          case '/' if(previous == '/') =>
            while(current != '\n' && source.hasNext) 
              readNext
            next
          
          case '*' if(previous == '/') =>
            def isEOComment(x: Char,y: Char) = (x == '*' && y == '/')
            while(!isEOComment(previous, current) && source.hasNext) {
              previous = current; readNext
            } // if comment not ended before EOF, then ok
            if(isEOComment(previous, current)) {
              readNext; next
            } else 
              fatal("Unterminated block comment.", new Token(BAD).setPos(f,source.pos))
          
          case _ => 
            if(previous == '=') 
              new Token(EQSIGN).setPos(f, position) 
            else if (previous == '/')
              new Token(DIV).setPos(f, position)
            else 
              new Token(BAD).setPos(f, position)
        }    
      }
      
      def intLit = {
        val position = source.pos
        sb.clear()
        while(current.isDigit) {
          sb.append(current); readNext
        }
        new INTLIT(sb.toInt).setPos(f, position)
      }
      
      def strLit = {
        val position = source.pos
        sb.clear(); readNext
        while(current != '"' && source.hasNext) {
          sb.append(current); readNext
        }
        if(source.hasNext)
          readNext; new STRLIT(sb.toString).setPos(f, position)
      }
    
      def identifierOrKeyword = {
        val position = source.pos
        sb.clear()
        while(current.isLetter || current.isDigit || current == '_') {
          sb.append(current); readNext
        }
        keywords.get(sb.toString) match {
          case None           =>   new ID(sb.toString).setPos(f, position)
          case Some(result)   =>   result.setPos(f, position)
        }
      }
      
      def endOfFile: Token = {
        // if file ends with }EOF then '}' hasn't been sent
        // otherwise current = whitespace
        if(current == '}') {
          current = ' '
          new Token(RBRACE).setPos(f, source.pos)
        }
        else {
          sentEOF = true
          new Token(EOF).setPos(f, source.pos)
        }
      }
      
      def isSingleSpecialChar  = {
        val specialChars = """[:;.,\!()\[\]{}<+-]""".r
        current match {
          case specialChars() => true
          case '*' => true
          case '%' => true
          case _ => false
        }
      }
      def isDoubleSpecialChar  = {
        val specialCase = "[=&|/]".r
        current match {
          case specialCase()  => true
          case _ => false
        }
      }
      def isWhitespace   = {
        val whitespace = """\s""".r;
        current match {
          case whitespace()   => true
          case _ => false
        }
      }
      
      val keywords = Map(
        "object"  -> new Token(OBJECT),
        "class"   -> new Token(CLASS),
        "def"     -> new Token(DEF),
        "var"     -> new Token(VAR),
        "Unit"    -> new Token(UNIT),
        "main"    -> new Token(MAIN),
        "String"  -> new Token(STRING ),
        "extends" -> new Token(EXTENDS),
        "Int"     -> new Token(INT),
        "Bool"    -> new Token(BOOLEAN),
        "while"   -> new Token(WHILE),
        "if"      -> new Token(IF),
        "else"    -> new Token(ELSE),
        "return"  -> new Token(RETURN),
        "length"  -> new Token(LENGTH),
        "true"    -> new Token(TRUE),
        "false"   -> new Token(FALSE),
        "this"    -> new Token(THIS),
        "new"     -> new Token(NEW),
        "println" -> new Token(PRINTLN))
      
      val specChar = Map(
          ":"     ->    new Token(COLON),
          ";"     ->    new Token(SEMICOLON),
          "."     ->    new Token(DOT),
          ","     ->    new Token(COMMA),
          "!"     ->    new Token(BANG),
          "("     ->    new Token(LPAREN),
          ")"     ->    new Token(RPAREN),
          "["     ->    new Token(LBRACKET),
          "]"     ->    new Token(RBRACKET),
          "{"     ->    new Token(LBRACE),
          "}"     ->    new Token(RBRACE),
          "<"     ->    new Token(LESSTHAN),
          "+"     ->    new Token(PLUS),
          "-"     ->    new Token(MINUS),
          "*"     ->    new Token(TIMES),
          "%"     ->    new Token(MOD))
    }
  }
}
