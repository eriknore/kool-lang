package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import koolc.analyzer.Types
import koolc.ast.Trees
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    // Some nice macros
    val TRUE = Ldc(1)
    val FALSE = Ldc(0)

    // Map names to ints for local variables, could be local and passed as an argument - which is better?
    var localVars: Map[String, Int] = Map.empty
    var currentClass: ClassDecl = new ClassDecl(Identifier("empty"), None, List.empty, List.empty)

    def convert(tpe: Type): String = {
      tpe match {
        case TInt => "I"
        case TString => "Ljava/lang/String;"
        case TBoolean => "Z"
        case TIntArray => "[I"
        case _ => "L" + tpe.toString + ";"
      }
    }
    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val parent = ct.parent match {
        case None => None
        case Some(p) => Some(p.value)
      }

      currentClass = ct

      val classFile = new ClassFile(ct.id.value, parent)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

//      parent match {
//        case None =>
//        case Some(p) => addParentStuff(classFile, p)
//      }

      // Add class memebers
      for (v <- ct.vars) { // TODO: Look for inherited members
        classFile.addField(convert(v.getSymbol.getType), v.id.value)
      }
      

      // Add method definitions and code
      for(m <- ct.methods) {
        var args: List[String] = List.empty
        for(arg <- m.args) args = convert(arg.getSymbol.getType) :: args
        val ch = classFile.addMethod(convert(m.retType.getType), m.id.value, args.reverse).codeHandler
        generateMethodCode(ch, m)
        ch.freeze
      }

      classFile.writeToFile(ct.id.value+".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      // Why would anyone leave this here?
      val methSym = mt.getSymbol

      // reserve slots for args
      for(i <- 0 until mt.args.length) {
        localVars += mt.args(i).id.value -> (i+1)
        ch.getFreshVar
      }

      val offset = mt.args.length

      // reserve slots for local variables
      for(i <-  0 until mt.vars.length) {
        localVars += mt.vars(i).id.value -> (i+1+offset)
        ch.getFreshVar
      }
      for(stmt <- mt.stats) generateStatCode(ch, stmt)

      // Generate return statement
      generateExprCode(ch, mt.retExpr)
      mt.retType.getType match {
        case TBoolean => ch << IRETURN
        case TInt => ch << IRETURN
        case _ => ch << ARETURN
      }

      // clear the list of local variables
      localVars = Map.empty
    }

    def getClassDeclFromName(classes: List[ClassDecl], name: String): ClassDecl = {
      // check empty list
      if(classes.isEmpty) {
        println(name)
        ??? // This should never happen :)
      }
      if(classes.head.id.value == name) classes.head
      else getClassDeclFromName(classes.tail, name)
    }

    def addParentStuff(classFile: ClassFile, p: String): Unit = {
      val cd: ClassDecl = getClassDeclFromName(prog.classes, p)
      cd.parent match {
        case None => // good, continue
        case Some(par) => addParentStuff(classFile, par.value)
      }

      // add parent vars to this class
      for (v <- cd.vars) {
        classFile.addField(convert(v.getSymbol.getType), v.id.value)
      }
      
//      // Add method definitions and code
//      for(m <- cd.methods) {
//        var args: List[String] = List.empty
//        for(arg <- m.args) args = convert(arg.getSymbol.getType) :: args
//        val ch = classFile.addMethod(convert(m.retType.getType), m.id.value, args.reverse).codeHandler
//        generateMethodCode(ch, m)
//        ch.freeze
//      }

    }


    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      // TODO: Emit code
      for (stmt <- stmts) generateStatCode(ch, stmt)
      ch << RETURN
      ch.freeze
    }

    def generateStatCode(ch: CodeHandler, stmt: StatTree): Unit = {
      stmt match {
        case b: Block => for(stmt <- b.stats) generateStatCode(ch, stmt)

        case i: If =>
          val elseLabel = ch.getFreshLabel("else")
          val returnLabel = ch.getFreshLabel("return")
          generateExprCode(ch, i.expr) // evaluate condition, only pops true if false
          ch << IfEq(elseLabel)
          generateStatCode(ch, i.thn)

          i.els match {
            case None => ch << Label(elseLabel) // we are done!
            case Some(stmt) =>
              ch << Goto(returnLabel) << Label(elseLabel)
              generateStatCode(ch, stmt)
          }

          ch << Label(returnLabel)

        case w: While =>
          val cond = ch.getFreshLabel("condition")
          val ret = ch.getFreshLabel("return")
          ch << Label(cond)
          generateExprCode(ch, w.expr) // evaluate condition, only pops true if false
          ch << IfEq(ret)
          generateStatCode(ch, w.stat)
          ch << Goto(cond) << Label(ret)

        case Println(expr: ExprTree) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          expr match {
            case p: Plus =>
              if(p.getType == TInt) {
                generateExprCode(ch, p)
                ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
              } else {
                generateExprCode(ch, p)
                ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
              }
            case _ =>
              generateExprCode(ch, expr)
              ch << InvokeVirtual("java/io/PrintStream", "println", "("+convert(expr.getType)+")V")
          }

        case Assign(target, expr) =>
          localVars.get(target.value) match {
            case Some(index: Int) => // variable is local
              target.getType match {
                case TBoolean =>
                  generateExprCode(ch, expr)
                  ch << IStore(index)
                case TInt =>
                  generateExprCode(ch, expr)
                  ch << IStore(index)
                case _ =>
                  generateExprCode(ch, expr)
                  ch << AStore(index)
              }
            case None => // variable MUST be a class field (?)
              ch << ALoad(0) // load 'this' before evaluate rhs to store in class member
              generateExprCode(ch, expr)
              val tpe = convert(target.getType)
              ch << PutField(currentClass.id.value, target.value, tpe)

          }

        case ArrayAssign(id, index, expr) =>
          generateExprCode(ch, id)
          generateExprCode(ch, index)
          generateExprCode(ch, expr)
          ch << IASTORE
      }
    }

    def generateExprCode(ch: CodeHandler, expr: ExprTree): Unit = {
      expr match {

        case And(lhs, rhs) =>
          val returnLabel = ch.getFreshLabel("and-return")
          ch << FALSE // assume false
          generateExprCode(ch, lhs)
          ch << IfEq(returnLabel) << POP
          generateExprCode(ch, rhs)
          ch << Label(returnLabel)

        case Or(lhs, rhs) =>
          val returnLabel = ch.getFreshLabel("or-return")
          ch << TRUE // assume true
          generateExprCode(ch, lhs)
          ch << IfNe(returnLabel) << POP
          generateExprCode(ch, rhs)
          ch << Label(returnLabel)

        case Equals(lhs, rhs) =>
          val returnLabel = ch.getFreshLabel("equals-return")
          ch << TRUE // assume true
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          if(lhs.getType == TInt || lhs.getType == TBoolean)
            ch << If_ICmpEq(returnLabel)
          else
            ch << If_ACmpEq(returnLabel)

          ch << POP << FALSE << Label(returnLabel)


        case LessThan(lhs, rhs) =>
          ch << TRUE // assume true
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          val returnLabel = ch.getFreshLabel("lt-return")
          ch << If_ICmpLt(returnLabel) << POP << FALSE << Label(returnLabel)

        case plus: Plus =>
          plus.getType match {
            case TInt =>
              generateExprCode(ch, plus.lhs)
              generateExprCode(ch, plus.rhs)
              ch << IADD
            case TString =>
              ch << DefaultNew("java/lang/StringBuilder")
              generateExprCode(ch, plus.lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + convert(plus.lhs.getType) + ")Ljava/lang/StringBuilder;")
              generateExprCode(ch, plus.rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + convert(plus.rhs.getType) + ")Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            case _ =>
              ??? // will never happen, error is caught earlier
          }

        case Minus(lhs, rhs) =>
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << ISUB

        case Mod(lhs, rhs) =>
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << IREM

        case Times(lhs, rhs) =>
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << IMUL

        case Div(lhs, rhs) =>
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << IDIV

        case ArrayRead(arr, index) =>
          generateExprCode(ch, arr)
          generateExprCode(ch, index)
          ch << IALOAD

        case ArrayLength(arr) =>
          generateExprCode(ch, arr)
          ch << ARRAYLENGTH

        case mc: MethodCall =>
          generateExprCode(ch, mc.obj)

          val (caller, meth) = mc.obj match {
            case t: This => findMethodOwner(currentClass.id.value, mc.meth.value)
            case ident: Identifier => findMethodOwner(ident.getType.toString, mc.meth.value)
            case New(ident: Identifier) => findMethodOwner(ident.getType.toString, mc.meth.value)
            case m: MethodCall =>
              //val nameOfObject = getTheClass(m)
              findMethodOwner(m.getType.toString, mc.meth.value)
            case _ => ??? // will never happen, no other type is possible - would be caught earlier

          }
          // create argument string
          val args = StringBuilder.newBuilder
          args.append("(")
          for(i <- 0 until mc.args.length) {
            generateExprCode(ch, mc.args(i))
            args.append(convert(meth.args(i).id.getType))
          }
          args.append(")")
          ch << InvokeVirtual(caller, mc.meth.value, args+convert(mc.getType))


        case i: IntLit => ch << Ldc(i.value)
        case str: StringLit => ch << Ldc(str.value)
        case True() => ch << TRUE // using macro
        case False() => ch << FALSE // using macro
        case ident: Identifier =>
          localVars.get(ident.value) match {
            case Some(index) =>
              ident.getType match {
                case TInt => ch << ILoad(index)
                case TBoolean => ch << ILoad(index)
                case _ => ch << ALoad(index)
              }
            case None =>
              ch << ALoad(0) // push 'this'
              val tpe = convert(ident.getType)
              ch << GetField(currentClass.id.value, ident.value, tpe)
          }

        case t: This => ch << ALoad(0)
        case New(ident) => ch << DefaultNew(ident.value)
        case NewIntArray(size) =>
          generateExprCode(ch, size)
          ch << NewArray(10) // 10 is code for Int - what did you think!?
        case Not(e) =>
          val returnLabel = ch.getFreshLabel("not-return")
          generateExprCode(ch, e)
          // SWAP changes place on top two values of stack?
          ch << TRUE << SWAP << IfEq(returnLabel) << POP << FALSE << Label(returnLabel)
        case _ => println(expr); ???
      }
    }

    def getTheClass(expr: ExprTree): String = {
      expr match {
        case MethodCall(obj,_,_) => getTheClass(obj)
        case New(ident: Identifier) => ident.getSymbol.name
        case ident: Identifier => println("returning "+ident.getType.toString); ident.getType.toString
        case t: This => currentClass.getSymbol.name
        case _ =>  ??? // Cannot happen, error caught earlier
      }
    }

    def findMethodOwner(cName: String, m: String): (String, MethodDecl) = {
      val c = getClassDeclFromName(prog.classes, cName)
      getMethod(c.methods, m) match {
        case Some(m) => (c.id.value, m)
        case None =>
          c.parent match {
            case None => println("Looking for " + m + " in " + c.id.value); ??? // Should never happen, someone must have declared the method!
            case Some(p) => findMethodOwner(p.value, m)
          }
      }
    }

    def getMethod(methods: List[MethodDecl], name: String): Option[MethodDecl] = {
      if(methods.isEmpty)
        None
      else {
        if(methods.head.id.value == name) Option(methods.head)
        else getMethod(methods.tail, name)
      }
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    // ...
    val mainClass = new ClassFile(prog.main.id.value, None) // main never has any parent
    mainClass.setSourceFile(sourceName)
    mainClass.addDefaultConstructor
    val ch = mainClass.addMainMethod.codeHandler
    generateMainMethodCode(ch, prog.main.stats, prog.main.id.value)

    // generate class file
    mainClass.writeToFile(outDir+"/"+prog.main.id.value+".class")
  }

}
