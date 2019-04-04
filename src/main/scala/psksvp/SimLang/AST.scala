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
// the simplest imperative language
// by psksvp@gmail.com
// Dec 12, 2015
// -----------------------------------------------
package psksvp.SimLang

object AST
{
  abstract class Type

  /////////////////
  case class NumericType() extends Type
  case class BooleanType() extends Type
  case class TextType() extends Type
  case class ArrayType(ctype:Type, size:Option[Int]) extends Type

  /////////////////
  abstract class Operator(val resultType: Type)
  abstract class NumericOperator extends Operator(NumericType())
  abstract class BooleanOperator extends Operator(BooleanType())

  case class Plus() extends NumericOperator
  case class Minus() extends NumericOperator
  case class Mul() extends NumericOperator
  case class Div() extends NumericOperator
  case class Power() extends NumericOperator
  case class Mod() extends NumericOperator
  case class Equal() extends BooleanOperator
  case class NotEqual() extends BooleanOperator
  case class Greater() extends BooleanOperator
  case class GreaterEqual() extends BooleanOperator
  case class Less() extends BooleanOperator
  case class LessEqual() extends BooleanOperator
  case class And() extends BooleanOperator
  case class Or() extends BooleanOperator
  case class Not() extends BooleanOperator

  /////////////////
  abstract class Term

  /////////////// expr ///////////////////////////////////
  abstract class Expr extends Term
  case class Literal(string: String) extends Expr
  case class Variable(name: String) extends Expr
  case class Binary(left: Expr, operator: Operator, right: Expr) extends Expr
  case class Unary(operator: Operator, expr: Expr) extends Expr
  case class FunctionCall(name: String, args: Seq[Expr]) extends Expr
  case class ArrayRef(variable:Variable, indexExpr:Expr) extends Expr

  ///////////////// value node ///////////////////////////
  abstract class Value extends Expr

  case class NumericValue(value: Float) extends Value
  {
    override def toString: String = value.toString
  }

  case class Fraction(a: Expr, b: Expr) extends Value

  case class BooleanValue(value: Boolean) extends Value
  {
    override def toString: String = value.toString
  }

  case class TextValue(value: String) extends Value
  {
    override def toString: String = value.toString
  }

  case class ArrayValue(value:Array[Value], ctype:Type) extends Value
  {
    def length:Int = value.length
    override def toString: String = s"[${value.mkString(",")}]"
  }

  /////////////////
  abstract class Statement extends Term

  case class Block(statements: Seq[Statement]) extends Statement
  case class VariableDeclaration(ptype: Type, id: String) extends Statement
  case class VariableDeclarationAssignment(id: String, expr: Expr) extends Statement
  case class Assignment(left:Expr, right:Expr) extends Statement
  case class If(expr: Expr, block: Block) extends Statement
  case class IfElse(expr: Expr, trueBlock: Block, falseBlock: Block) extends Statement
  case class While(expr: Expr, block: Block) extends Statement
  case class ProcedureCall(function:FunctionCall) extends Statement
  ///////////////////////////

  case class FunctionSignature(name: String, rtype:Type, args: Seq[VariableDeclaration]) extends Term
  case class FunctionDef(signature:FunctionSignature, body: Block) extends Term
  case class Program(functions:Seq[FunctionDef]) extends Term

  val mainSignature:FunctionSignature = FunctionSignature("main", NumericType(), Nil)
}