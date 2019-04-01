/*
 *  The BSD 3-Clause License
 *  Copyright (c) 2018. by Pongsak Suvanpong (psksvp@gmail.com)
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *  this list of conditions and the following disclaimer in the documentation
 *  and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its contributors may
 *  be used to endorse or promote products derived from this software without
 *  specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 *  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This information is provided for personal educational purposes only.
 *
 * The author does not guarantee the accuracy of this information.
 *
 * By using the provided information, libraries or software, you solely take the risks of damaging your hardwares.
 */
package psksvp.SimLang

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}


/**
  * | is the alternation combinator. It says “succeed if either the left or right operand parse successfully”
  * ~ is the sequential combinator. It says “succeed if the left operand parses successfully, and then the right parses successfully on the remaining input”
  * ~> says “succeed if the left operand parses successfully followed by the right, but do not include the left content in the result”
  * <~ is the reverse, “succeed if the left operand is parsed successfully followed by the right, but do not include the right content in the result”
  * ^^=> is the transformation combinator. It says “if the left operand parses successfully, transform the result using the function on the right”
  * rep => simply says “expect N-many repetitions of parser X” where X is the parser passed as an argument to rep
  */

class Parser extends JavaTokenParsers with PackratParsers
{
  import AST._

  implicit def toOp(s:String):Operator = s match
  {
    case "+" => Plus()
    case "-" => Minus()
    case "*" => Mul()
    case "/" => Div()
    case "%" => Mod()
    case "==" => Equal()
    case "!=" => NotEqual()
    case ">"  => Greater()
    case ">=" => GreaterEqual()
    case "<"  => Less()
    case "<=" => LessEqual()
    case "&&" => And()
    case "||" => Or()
    case "!"  => Not()

    case _    => sys.error(s"psksvp.SimLang.Parser.toOp don't know symbol $s")
  }

  lazy val valueLit:PackratParser[Literal] = (stringLiteral | "true" | "false" | floatingPointNumber | wholeNumber | decimalNumber) ^^
  {
    s => Literal(s)
  }


  lazy val variable:PackratParser[Variable] = ident ^^ {v => Variable(v)}

  lazy val varType:PackratParser[Type] = ("numeric" | "boolean" | "text") ^^
  {
    s => s match
         {
           case "numeric" => NumericType()
           case "boolean" => BooleanType()
           case "text"    => TextType()
         }
  }

  lazy val functionCall:PackratParser[FunctionCall] = rep1sep(ident, ".") ~ ("(" ~> exprs <~ ")") ^^
  {
    case l ~ p => FunctionCall(l.mkString("."), p)
  }


  lazy val uopDef:PackratParser[String] = "-" | "+" | "!"
  lazy val bopP12Def:PackratParser[String] = "*" | "/" | "%"
  lazy val bopP11Def:PackratParser[String] = "-" | "+" | ">>>" | "<<" | ">>"  | "&" | "|" | "^"
  lazy val bopCompDef:PackratParser[String] = "==" | "!=" | ">=" | "<=" | ">" | "<"
  lazy val bopLogicDef:PackratParser[String] = "&&" | "||"

  lazy val mexpDef:PackratParser[Expr] =
    mexpDef ~ bopLogicDef ~ mexp2Def ^^ {case expL ~ op ~ expR => Binary( expL, op,  expR)} |
      mexp2Def

  lazy val mexp2Def:PackratParser[Expr] =
    mexp1Def ~ bopCompDef ~ mexp1Def ^^ {case expL ~ op ~ expR => Binary(expL,  op, expR)} |
      mexp1Def

  lazy val mexp1Def:PackratParser[Expr] =
    mexp1Def ~ bopP11Def ~ mexp0Def ^^ {case expL ~ op ~ expR => Binary(expL,  op, expR)} |
      mexp0Def

  lazy val mexp0Def:PackratParser[Expr] =
    mexp0Def ~ bopP12Def ~ factor ^^ {case expL ~ op ~ expR => Binary(expL,  op, expR)} |
      factor

  lazy val factor:PackratParser[Expr] =
    functionCall | valueLit  |
    ident ^^ {v => Variable(v)} |
    uopDef ~ mexpDef  ^^ {case op ~ exp => Unary(op, exp)} |
    "(" ~> mexpDef <~ ")"

  /*
  import scala.util.parsing.combinator._

  class Arith extends JavaTokenParsers {
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
  }
   */


  lazy val expr:PackratParser[Expr] = mexpDef
  lazy val exprs:PackratParser[Seq[Expr]] = repsep(expr, ",")
  lazy val paramList:PackratParser[Seq[VariableDeclaration]] = repsep(paramDecl, ",")

  ///// statements
  lazy val statement: PackratParser[Statement] = block | print | ifElse | ifCond | whileLoop | procedureCall |
                                                 varDeclAssignment| varDecl | assignment

  lazy val block:PackratParser[Block] = "{" ~> (statement *) <~ "}" ^^
  {
    s => Block(s)
  }

  lazy val print:PackratParser[Print] = "sys.print" ~> ("(" ~> exprs <~ ")")  ^^
  {
    e => Print(e)
  }

  lazy val ifElse:PackratParser[IfElse] = "if" ~> ("(" ~> expr <~ ")") ~ statement ~ ("else" ~> statement) ^^
  {
    case cond ~ tstmt ~ fstmt => IfElse(cond, Block(Seq(tstmt)), Block(Seq(fstmt)))
  }

  lazy val ifCond:PackratParser[If] = "if" ~> ("(" ~> expr <~ ")") ~ statement ^^
  {
    case cond ~ stmt => If(cond, Block(Seq(stmt)))
  }

  lazy val whileLoop:PackratParser[While] = "while" ~> ("(" ~> expr <~ ")") ~ statement ^^
  {
    case cond ~ stmt => While(cond, Block(Seq(stmt)))
  }

  lazy val varDecl:PackratParser[VariableDeclaration] = ("var" ~> ident <~ ":") ~ varType ^^
  {
    case id ~ ty => VariableDeclaration(ty, id)
  }

  lazy val paramDecl:PackratParser[VariableDeclaration] = (ident <~ ":") ~ varType ^^
    {
      case id ~ ty => VariableDeclaration(ty, id)
    }

  lazy val varDeclAssignment:PackratParser[VariableDeclarationAssignment] = (varDecl <~ "=") ~ expr  ^^
  {
    case decl ~ e => VariableDeclarationAssignment(decl.ptype, decl.name, e)
  }

  lazy val assignment:PackratParser[Assignment] = (ident <~ "=") ~ expr ^^
  {
    case id ~ e => Assignment(Variable(id), e)
  }

  lazy val procedureCall:PackratParser[ProcedureCall] = functionCall ^^
  {
    f => ProcedureCall(f)
  }

  lazy val functionSignature:PackratParser[FunctionSignature] = ("function" ~> ident) ~ ("(" ~> paramList <~ ")") ~ (":" ~> varType) ^^
  {
    case n ~ p ~ t => FunctionSignature(n, t, p)
  }

  lazy val functionDef:PackratParser[FunctionDef] =  functionSignature ~ block ^^
  {
    case sign ~ b => FunctionDef(sign, b)
  }


  lazy val program:PackratParser[Program] = (functionDef *) ^^
  {
    fs => Program(fs)
  }

  lazy val functionModifier:PackratParser[String] = "function" | "monitor" | "task"
}


