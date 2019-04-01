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

object Entry
{

  def main(args:Array[String]):Unit=
  {
    parseFuncDef()
  }

  def parseFuncDef():Unit =
  {
    val src =
      """
        |function main():numeric
        |{
        |  var m:numeric = sum(23, 45)
        |  var g:numeric = math.cos(3.14)
        |  sys.print(m)
        |  sys.print(g)
        |  sys.print(sum(10))
        |  sys.print(rsum(10))
        |
        |  sys.print("sin of pi/2 is", math.sin(3.14 / 2))
        |  sys.print(bah(1))
        |  scope()
        |  main = 0
        |}
        |
        |function sum(a:numeric, b:numeric):numeric
        |{
        |  sum = a + b
        |}
        |
        |function sum(a:numeric):numeric
        |{
        |  sum = 0
        |  var n:numeric = 1
        |  while(n <= a)
        |  {
        |    sum = sum + n
        |    n = n + 1
        |  }
        |}
        |
        |function rsum(a:numeric):numeric
        |{
        |  if(a == 0)
        |    rsum = 0
        |  else
        |  {
        |    rsum = a + rsum(a - 1)
        |  }
        |}
        |
        |function bah(m:numeric):text
        |{
        |  bah = "HelloWorld"
        |}
        |
        |function scope():numeric
        |{
        |  var m:text = "Hello"
        |  {
        |    var m:text = "world"
        |    {
        |      var m:numeric = 85
        |      sys.print(m)
        |    }
        |    sys.print(m)
        |  }
        |  sys.print(m)
        |}
      """.stripMargin


    val parser = new Parser()
    val p = parser.parseAll(parser.program, src).get
    println(p)

    println(Interpreter.run(p))
  }
}