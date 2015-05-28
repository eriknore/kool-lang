package koolc

import koolc.ast.Trees.Program
import utils._
import java.io.File

import lexer._
import ast._
import analyzer._
import code._

object Main {
  var tokensFlag = false
  var astFlag = false
  var symIDFlag = false
  var prettyFlag = false
  var bnfc = false
  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case "--tokens" :: args =>
        tokensFlag = true
        processOption(args)

      case "--bnfc" :: args =>
        bnfc = true
        processOption(args)

      case "--ast" :: args =>
        astFlag = true
        processOption(args)

      case "--symid" :: args =>
        symIDFlag = true
        processOption(args)

      case "--pretty" :: args =>
        prettyFlag = true
        processOption(args)

      case f ::args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    if (tokensFlag) {
      // prints all tokens if flag "--tokens" is present
      val pipeline = Lexer andThen PrintTokens
      val program = pipeline.run(ctx)(ctx.file)
      for (token <- program) token
    } else {
      var pipelineFrontEnd: Pipeline[File, Program] = null

      // if flag --bnfc is given run the new external BNFC lexer and parser
      if (bnfc) pipelineFrontEnd = AstConverter
      // otherwise run original Lexer + Parser
      else pipelineFrontEnd = Lexer andThen Parser

      if (astFlag) {
        if(symIDFlag)
          pipelineFrontEnd = pipelineFrontEnd andThen NameAnalysis
        val program = pipelineFrontEnd.run(ctx)(ctx.file)
        if (symIDFlag) println(SymIDPrinter(program))
        else println(program)
      } else if (prettyFlag) {
        if(symIDFlag)
          pipelineFrontEnd = pipelineFrontEnd andThen NameAnalysis
        val program = pipelineFrontEnd.run(ctx)(ctx.file)
        println(Printer(program, symIDFlag))
      } else { // generate code
        val pipeline =  pipelineFrontEnd andThen
                        NameAnalysis andThen 
                        TypeChecking andThen
                        CodeGeneration
        pipeline.run(ctx)(ctx.file)
      }
    }
  }
}
