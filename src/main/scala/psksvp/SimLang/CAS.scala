/*
 *  The BSD 3-Clause License
 *  Copyright (c) 2019. by Pongsak Suvanpong (psksvp@gmail.com)
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

import AST._

object CAS
{
  class DSLExpr(val e:Expr)
  {
    def +(other:Expr):Expr = Binary(e, Plus(), other)
    def -(other:Expr):Expr = Binary(e, Minus(), other)
    def *(other:Expr):Expr = Binary(e, Mul(), other)
    def /(other:Expr):Expr = Binary(e, Div(), other)
    def ^(other:Expr):Expr = Binary(e, Power(), other)

    def unary_-():Expr = Unary(Minus(), e)
    def unary_+():Expr = e
  }

  case class sin(e:Expr) extends Expr
  case class cos(e:Expr) extends Expr
  case class ln(e:Expr) extends Expr

  
  implicit def toExpression(e:Expr):DSLExpr = new DSLExpr(e)
  implicit def toNum(n:Int):NumericValue = NumericValue(n)
  implicit def toNum(n:Double):NumericValue = NumericValue(n.toFloat)

  // def simplify(e:Expr):Expr = e match
  // {
  //   case ()
  // }

 def indexLaw(e:Expr):Expr =
 {
   case Binary(Binary(x1, Power(), a), Div(), Binary(x2, Power(), b))
        if x1 == x2 => (x1 ^ (a - b))
 }


}


