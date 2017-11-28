// the simplest imperative language
// by psksvp@gmail.com
// Dec 12, 2015
// -----------------------------------------------
abstract class Type
case class NumericType() extends Type
case class BooleanType() extends Type
case class TextType() extends Type

abstract class Operator(val resultType:Type)
abstract class NumericOperator extends Operator(NumericType())
abstract class BooleanOperator extends Operator(BooleanType())
 
case class Plus() extends NumericOperator
case class Minus() extends NumericOperator
case class Mul() extends NumericOperator
case class Div() extends NumericOperator
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


abstract class Term
abstract class Expr extends Term
case class Literal(string:String) extends Expr
case class Variable(name:String) extends Expr
case class Binary(left:Expr, operator:Operator, right:Expr) extends Expr
case class Unary(operator:Operator, expr:Expr) extends Expr

case class functionCall(name:String, args:Seq[Expr]) extends Expr

abstract class Value extends Expr
case class NumericValue(value:Float) extends Value
case class BooleanValue(value:Boolean) extends Value
case class TextValue(value:String) extends Value

abstract class Statement extends Term
case class Block(statements:Seq[Statement]) extends Statement
case class VariableDeclaration(ptype:Type, name:String) extends Statement
case class VariableDeclarationAssignment(ptype:Type, name:String, expr:Expr) extends Statement
case class Assignment(variable:Variable, expr:Expr) extends Statement
case class If(expr:Expr, block:Block) extends Statement
case class IfElse(expr:Expr, trueBlock:Block, falseBlock:Block) extends Statement
case class While(expr:Expr, block:Block) extends Statement
case class Print(expr:Expr) extends Statement

case class Program(name:String, block:Block) extends Term

object Interpreter
{
  import scala.collection.mutable.Map
  
  type Register = Map[String, Type]
  type Memory = Map[String, Value]
  
  def apply():Interpreter = new Interpreter
  
  def apply(program:Program):(Memory, Register) = (new Interpreter).execute(program)
  
  def literal2Value(l:String):Value = 
  {
    try
    {
      NumericValue(l.toFloat)
    }
    catch
    {
      case _:Exception => try
                          {
                            BooleanValue(l.toBoolean)
                          }
                          catch
                          {
                            case _:Exception => TextValue(l)
                          }
    }
    
  }
  
  def typeCheck(t:Type, v:Value):Boolean = 
  {
    (t, v) match
    {
      case (_:NumericType, _:NumericValue) => true
      case (_:BooleanType, _:BooleanValue) => true
      case (_:TextType, _:TextValue)       => true
      case _                               => false
    } 
  }
}

class Interpreter
{
  import scala.collection.mutable.Map
  import Interpreter._
  
  val register = Map[String, Type]()
  val memory = Map[String, Value]()
  
  def execute(expr:Expr): Value = expr match
  {
    case Literal(l)                                                 => literal2Value(l)
    case Variable(n)                                                => memory(n)
    case Binary(l:NumericValue, op:NumericOperator, r:NumericValue) => compute(l, op, r)
    case Binary(l:NumericValue, op:BooleanOperator, r:NumericValue) => compute(l, op, r)
    case Binary(l:BooleanValue, op:BooleanOperator, r:BooleanValue) => compute(l, op, r)
    case Binary(l, op, r)                                           => val left = execute(l)
                                                                       val right = execute(r)
                                                                       execute(Binary(left, op, right))
    case Unary(op:Plus, v:NumericValue)                             => NumericValue(+v.value)
    case Unary(op:Minus, v:NumericValue)                            => NumericValue(-v.value)
    case Unary(op:Not, v:BooleanValue)                              => BooleanValue(!v.value)
    case Unary(op, e)                                               => val v = execute(e)
                                                                       execute(Unary(op, v))
    case _                                                          => sys.error(s"error executing $expr")
  }
  
  def execute(statement:Statement): Unit = statement match
  {
    case Block(statements)                      => for(s <- statements) execute(s)
    case VariableDeclaration(t, n)              => if(!register.contains(n))
                                                     register(n) = t
                                                   else
                                                     sys.error(s"duplicate variable $n")   
                                                     
    case VariableDeclarationAssignment(t, n, e) => execute(VariableDeclaration(t, n))
                                                   execute(Assignment(Variable(n), e))
                                                   
    case Assignment(v, e)                       => if(register.contains(v.name))
                                                   {
                                                     val ev = execute(e)
                                                     if(typeCheck(register(v.name), ev)) 
                                                       memory(v.name) = ev  
                                                     else
                                                       sys.error(s"type mismatch at $statement")        
                                                   }
                                                   else
                                                    sys.error(s"$v is used without declaration")
     
    case If(e:BooleanValue, b)                  => if(e.value) execute(b)
    case IfElse(e:BooleanValue, tBlock, fBlock) => if(e.value) 
                                                     execute(tBlock)
                                                   else 
                                                     execute(fBlock)                         
    case If(e, b)                               => val v = execute(e)
                                                   execute(If(v, b))  
    
    case IfElse(e, tBlock, fBlock)              => val v = execute(e)
                                                   execute(IfElse(v, tBlock, fBlock))
                     
    case While(e, b)                            => execute(e) match
                                                   {
                                                     case BooleanValue(cond) => if(cond) 
                                                                                {
                                                                                  execute(b)
                                                                                  execute(While(e, b))
                                                                                }
                                                     case _                  => sys.error(s"syntax error at $statement")                              
                                                   } 
                                                   
    case Print(e)                                => println(execute(e))                                                      
    case _                                       => sys.error(s"syntax error at $statement")                 
  }
  
  def execute(program:Program):(Memory, Register) =
  {
    execute(program.block)
    (memory, register)
  }
  
  def compute(left:NumericValue, 
                op:NumericOperator, 
             right:NumericValue):NumericValue = op match
  {
    case Plus()  => NumericValue(left.value + right.value)
    case Minus() => NumericValue(left.value - right.value)
    case Mul()   => NumericValue(left.value * right.value)
    case Div()   => NumericValue(left.value / right.value)
    case Mod()   => NumericValue(left.value.toInt % right.value.toInt)
  }
  
  def compute(left:NumericValue, 
                op:BooleanOperator, 
             right:NumericValue):BooleanValue = op match
  {
    case Equal()        => BooleanValue(left.value == right.value)
    case NotEqual()     => BooleanValue(left.value != right.value)
    case Greater()      => BooleanValue(left.value > right.value)
    case GreaterEqual() => BooleanValue(left.value >= right.value)
    case Less()         => BooleanValue(left.value < right.value)
    case LessEqual()    => BooleanValue(left.value.toInt <= right.value.toInt)
  }
  
  def compute(left:BooleanValue, 
                op:BooleanOperator, 
             right:BooleanValue):BooleanValue = op match
  {
    case And() => BooleanValue(left.value && right.value)
    case Or()  => BooleanValue(left.value || right.value)
    case _     => sys.error("NOT is not a binary op")
  }
  
  
}

object EntryPoint
{
  def main(args:Array[String]):Unit=
  {
    println(Interpreter.literal2Value("1.2dfss3"))
    println(Interpreter.literal2Value("1.23"))
    println(Interpreter.literal2Value("1"))
    println(Interpreter.literal2Value("-1"))
    println(Interpreter.literal2Value("-3.1412"))
    println(Interpreter.literal2Value("true"))
    println(Interpreter.literal2Value("false"))
    
    val i = Interpreter()
    val e1 = Binary(Literal("1"), Plus(), Literal("2"))
    println(i.execute(e1))
    val e2 = Binary(e1, Mul(), Literal("4"))
    println(i.execute(e2))
    val e3 = Binary(e2, Less(), e1)
    println(i.execute(e3))
    val e4 = Unary(Not(), e3)
    println(i.execute(e4))
    val e5 = Binary(e4, Or(), e3)
    println(i.execute(e5))
    val e6 = Binary(e4, And(), e3)
    println(i.execute(e6))
    
    val s1 = Seq(
      VariableDeclaration(NumericType(), "h"),
      Assignment(Variable("h"), e1),
      VariableDeclarationAssignment(BooleanType(), "b", Binary(e2, Greater(), Variable("h"))),
      While(Binary(Variable("h"), Less(), e2),
            Block(
              Seq(
                  Print(Variable("h")),
                  IfElse(Binary(Binary(Variable("h"), Mod(), Literal("2")), Equal(), Literal("0")), 
                         Block(Seq(Print(Literal("even")))),
                         Block(Seq(Print(Literal("odd"))))
                        ),
                  Assignment(Variable("h"), Binary(Variable("h"), Plus(), Literal("1")))      
                 ) 
                )
          )
    )
    val p1 = Program("HelloWorld", Block(s1))
    println(Interpreter(p1))
  }
}