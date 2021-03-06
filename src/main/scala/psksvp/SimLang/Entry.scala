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
    parseArray()
    parseFuncDef()
  }

  def parseArray():Unit =
  {
    val s =
      """
        |function main():numeric
        |{
        |  var a:array<numeric>(10)
        |  sys.print(a[9])
        |  sys.print(a)
        |  a[9] = 9
        |  sys.print(a)
        |  var i = 0
        |  while(i < 9)
        |  {
        |    a[i + 1] = i * 2
        |    i = i + 1
        |  }
        |  sys.print(a)
        |  sys.print("The length of array a is ", sys.length(a))
        |  sys.print("The length of text helloworld is", sys.length("helloworld"))
        |
        |
        |  var b:array<numeric>
        |
        |  b = [1, 2, 3, 4]
        |  sys.print(b)
        |  sys.print("The length of b is ", sys.length(b))
        |
        |  var c = ["Hello", "World"]
        |  sys.print(c)
        |
        |  var strc = "Pongsak"
        |  sys.print(strc[5])
        |
        |  sys.print(sum(b))
        |  sys.print(b)
        |
        |  var re = text.concat("hello ", "world ", "of ", "code", 100, 10.20, false)
        |
        |  sys.print(re)
        |
        |  randomA(b)
        |  sys.print(b)
        |  sys.print("----------------")
        |
        |  var ra = randomGen(6)
        |  sys.print(ra)
        |
        |  sys.print("----------------")
        |  var hkl = text.concat("12343", 1234)
        |  sys.print(hkl)
        |}
        |
        |function sum(a:array<numeric>):numeric
        |{
        |  sum = 0
        |  var i = 0
        |  while(i < sys.length(a))
        |  {
        |    sum = sum + a[i]
        |    i = i + 1
        |  }
        |  a[0] = 20
        |}
        |
        |function randomA(a:array<numeric>):numeric
        |{
        |  var i = 0
        |  while(i < sys.length(a))
        |  {
        |    a[i] = sys.random()
        |    i = i + 1
        |  }
        |}
        |
        |function randomGen(a:numeric):array<numeric>
        |{
        |  var result:array<numeric>(a)
        |  var i = 0
        |  while(i < sys.length(result))
        |  {
        |    result[i] = sys.random()
        |    i = i + 1
        |  }
        |  randomGen = result
        |}
      """.stripMargin


    val p = new Parser().parseProgram(s)
    println(p)
    println(Interpreter.run(p))
  }

  def parseFuncDef():Unit =
  {
    val src =
      """
        |function main():numeric
        |{
        |  var m = sum(23, 45)
        |  var g = math.cos(3.14)
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
        |  var n = 1
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
        |  var m = "Hello"
        |  {
        |    var m = "world"
        |    {
        |      var m = 85
        |      sys.print(m)
        |    }
        |    sys.print(m)
        |  }
        |  sys.print(m)
        |}
      """.stripMargin


    val p = new Parser().parseProgram(src)
    println(p)
    println(Interpreter.run(p))
  }

  def casPlay():Unit =
  {
    import CAS._

    //val e = (Var("x") + 5) * Var("y")
    println(e)
  }
}
