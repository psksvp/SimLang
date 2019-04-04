# SimLang the simplest imperative language (I think).
by psksvp@gmail.com (pongsak suvanpong)

I wrote this a few year back to learn Scala programming language and to learn how language processing works (how to implement a language). So I decided to implement a simple imperative language which I call *SimLang*. The syntax of the language was taken from C, Java, Scala and Pascal language. However inner function is not supported. There is no user defined type and object.

Currently SimLang is not a compiled language. It requires its semantic interpreter to execute a SimLang program. 

## Disclaimer
 
This is a toy language, there are bugs, flaws in the implementation.

## SimLang

### Program

In SimLang, a program is a collection of functions. A program entry point is the function main.

~~~
function main():numeric
{
  sys.print("HelloWorld")
}
~~~

### Data Type 

SimLang support the following data type.

* *numeric* is 32 bits wide which can be both real and whole number.
* *boolean* : true, false
* *text* : sequence of characters (aka string). Access to each char in a text uses array notation.
* *array* : sequence of the numeric or boolean or text.

~~~
function main():numeric
{
  var a:numeric
  var b:boolean
  var c:text
  
  a = 20
  b = a != a
  c = "HelloWorld"
  
  sys.print(c[0])
  
  var m:Array<text>(3)
  m[0] = "plane"
  m[1] = "train"
  m[2] = "automobile"
}
~~~

The code below shows variables declaration with type infer from the right hand side of the assignment symbol (=)

~~~
function main():numeric
{
  var a = 10
  var b = 3.14142
  var m = "Hello World"
  var g = a == b
  var k = !(a == b)
  
  var ls = [1, 3.14142, 5, 7, 9, 99.9]
  var lm = ["cat", "dog", "bird"]
  
  sys.print("length of lm is ", sys.length(lm))
  sys.print("length of lm[0] is ", sys.length(lm[0]))
  sys.print("length of ls[1] is ", sys.length(ls[1]))
}
~~~

### Operators

* +, -, *, / 
* % for modulo
* == for equality
* >, <, >=, <=, !=
* &&, ||

### Conditional

The conditional statement is very similar to that of Java and C. 

~~~
function main():numeric
{
  var a = sys.random()
  if(a >= 0.5)
  {
    sys.print("winning")
  }
  else
    sys.print("better luck next time")
}
~~~

### Loop

*while* is the only loop form in SimLang.

~~~
function main():numeric
{
  var a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  var i = 0
  var sum = 0
  while(i < sys.length(a))
  {
    sum = sum + a[i]
    i = i + 1
  }
}
~~~

### Function

A function in SimLang can have zero or more parameter. It **must** produce a value of any of the supported types. 

The function **main** which is the entry point of a program *must* produce a *numeric* value type. The function main in the code listing below produces NumericValue(0) by default.

~~~
function main():numeric
{
  sys.print("HelloWorld")
}
~~~

The returned value of a function is stored in a variable which has the same identifier as the name of the function. The variable is automatically added to the function scope by default. The type of the variable is the same as the function. The example below shows function main produces a 404 numeric value NumericValue(404).

~~~
function main():numeric
{
  sys.print("HelloWorld")
  main = 404
}
~~~

An array is passed to a function by reference.

~~~
function main():numeric
{
  var b = [1, 2, 3, 4]
  sys.print(b)
  randomA(b)
  sys.print(b)
}

function randomA(a:array<numeric>):numeric
{
  var i = 0
  while(i < sys.length(a))
  {
    a[i] = sys.random()
    i = i + 1
  }
}
~~~ 

### Recursion

Recursion is supported. The example below shows a recursion function which sums the number from 1 to a.

~~~
function sum(a:numeric):numeric
{
  if(0 >= a)
    sum = 0
  else
    sum = a + sum(a - 1)  
}
~~~

### Built-in functions

* math
	* math.cos
	* math.sin
	* math.tan
	* math.sqrt
	* math.power
	
* sys
 	* sys.print
 	* sys.length
 	* sys.random

* text
 	* text.concat
 	* text.sub

## Examples

~~~
function main():numeric
{
  var a:array<numeric>(10)
  sys.print(a[9])
  sys.print(a)
  a[9] = 9
  sys.print(a)
  var i = 0
  while(i < 9)
  {
    a[i + 1] = i * 2
    i = i + 1
  }
  sys.print(a)
  sys.print("The length of array a is ", sys.length(a))
  sys.print("The length of text helloworld is", sys.length("helloworld"))


  var b:array<numeric>

  b = [1, 2, 3, 4]
  sys.print(b)
  sys.print("The length of b is ", sys.length(b))

  var c = ["Hello", "World"]
  sys.print(c)

  var strc = "Pongsak"
  sys.print(strc[5])

  sys.print(sum(b))
  sys.print(b)

  var re = text.concat("hello ", "world ", "of ", "code", 100, 10.20, false)

  sys.print(re)
}

function sum(a:array<numeric>):numeric
{
  sum = 0
  var i = 0
  while(i < sys.length(a))
  {
    sum = sum + a[i]
    i = i + 1
  }
  a[0] = 20
}
~~~

~~~
function main():numeric
{
  var m = sum(23, 45)
  var g = math.cos(3.14)
  sys.print(m)
  sys.print(g)
  sys.print(sum(10))
  sys.print(rsum(10))

  sys.print("sin of pi/2 is", math.sin(3.14 / 2))
  sys.print(bah(1))
  scope()
  main = 0
}

function sum(a:numeric, b:numeric):numeric
{
  sum = a + b
}

function sum(a:numeric):numeric
{
  sum = 0
  var n = 1
  while(n <= a)
  {
    sum = sum + n
    n = n + 1
  }
}

function rsum(a:numeric):numeric
{
  if(a <= 0)
    rsum = 0
  else
  {
    rsum = a + rsum(a - 1)
  }
}

function bah(m:numeric):text
{
  bah = "HelloWorld"
}

function scope():numeric
{
  var m = "Hello"
  {
    var m = "world"
    {
      var m = 85
      sys.print(m)
    }
    sys.print(m)
  }
  sys.print(m)
}
~~~





 


