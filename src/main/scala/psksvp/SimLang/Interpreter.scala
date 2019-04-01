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

object Interpreter
{
  import AST._

  class Memory
  {
    private val data = scala.collection.mutable.Map[String, (Type, Seq[Value])]()

    def has(name:String):Boolean = data.contains(name)

    def register(name:String, tpy:Type):Unit =
    {
      data(name) = (tpy, Seq(defaultValue(tpy)))
    }

    def update(name:String, value:Value):Unit = data.get(name) match
    {
      case Some((t, s)) => data(name) = (t, value +: s)
      case None         => sys.error(s"memory write error, Variable($name) does not exist")
    }

    def apply(name:String):Value = getValue(name)

    def getValue(name:String):Value = data.get(name) match
    {
      case Some((_, c :: _ )) => c
      case None               => sys.error(s"memory read value error, Variable($name) does not exist")
    }

    def getValues(name:String):Seq[Value] = data.get(name) match
    {
      case Some((_, v )) => v
      case None          => sys.error(s"memory read value error, Variable($name) does not exist")
    }

    def getType(name:String):Type = data.get(name) match
    {
      case Some((t, _ )) => t
      case None          => sys.error(s"memory read type error, Variable($name) does not exist")
    }
  }

  case class Environment(memory:Memory = new Memory())
  

  def findFunction(signature:FunctionSignature, program:Program):Option[FunctionDef] =
  {
    program.functions.find(_.signature == signature)
  }

  def findFunction(name:String, params:Seq[Value], program: Program):Option[FunctionDef] =
  {
    program.functions.find
    {
      f => f.signature.name == name &&
           f.signature.args.length == params.length &&
           (if(f.signature.args.nonEmpty)
             (for((t, v) <- f.signature.args zip params) yield typeAgree(t.ptype, v)).reduceLeft(_ && _)
           else
             true)

    }
  }

  def findMainFunction(program: Program):Option[FunctionDef] =
  {
    findFunction(AST.mainSignature, program)
  }

  def apply(program:Program):Option[Value] =
  {
    findMainFunction(program) match
    {
      case Some(main) => Some(new Interpreter(program).invokeFunction(main, Nil))
      case None       => None
    }
  }

  def run(program:Program):Option[Value] = apply(program)
  
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
  
  def typeAgree(t:Type, v:Value):Boolean =
  {
    (t, v) match
    {
      case (_:NumericType, _:NumericValue) => true
      case (_:BooleanType, _:BooleanValue) => true
      case (_:TextType, _:TextValue)       => true
      case _                               => false
    }
  }

  def defaultValue(t:Type):Value = t match
  {
    case NumericType() => NumericValue(0)
    case TextType()    => TextValue("")
    case BooleanType() => BooleanValue(true)
  }
}

/**
  *
  * @param program
  */
class Interpreter(program:AST.Program)
{
  import Interpreter._
  import AST._

  private var envs:Seq[Environment] = Nil //Seq(Environment())

  private def top:Environment = envs.head
  private def push():Unit = envs = Environment() +: envs
  private def pop():Unit = envs = envs.tail

  private def lookupVar(s:String):Option[Environment] = envs.find{e => e.memory.has(s)}

  def dump():Unit = envs.foreach(println(_))

  def evaluate(expr:Expr): Value = expr match
  {
    case Literal(l)                                                 => literal2Value(l)
    case Variable(n)                                                => lookupVar(n) match
                                                                       {
                                                                         case Some(env) => env.memory(n)
                                                                         case None      => sys.error(s"$expr is used without declaration")
                                                                       }
    case Binary(l:NumericValue, op:NumericOperator, r:NumericValue) => compute(l, op, r)
    case Binary(l:NumericValue, op:BooleanOperator, r:NumericValue) => compute(l, op, r)
    case Binary(l:BooleanValue, op:BooleanOperator, r:BooleanValue) => compute(l, op, r)
    case Binary(l, op, r)                                           => evaluate(Binary(evaluate(l), op, evaluate(r)))
    case Unary(op:Plus, v:NumericValue)                             => NumericValue(+v.value)
    case Unary(op:Minus, v:NumericValue)                            => NumericValue(-v.value)
    case Unary(op:Not, v:BooleanValue)                              => BooleanValue(!v.value)
    case Unary(op, e)                                               => evaluate(Unary(op, evaluate(e)))
    case FunctionCall(n, parms)                                     => val p = for(e <- parms) yield evaluate(e)
                                                                       invokeFunction(n, p)
    case _                                                          => sys.error(s"error executing $expr")
  }

  def execute(statement:Statement): Unit = statement match
  {
    case Block(statements)                      => push()
                                                   for(s <- statements) execute(s)
                                                   pop()

    case VariableDeclaration(t, n)              => if(!top.memory.has(n))
                                                     top.memory.register(n, t)
                                                   else
                                                     sys.error(s"duplicate variable $n")

    case VariableDeclarationAssignment(t, n, e) => execute(VariableDeclaration(t, n))
                                                   execute(Assignment(Variable(n), e))

    case Assignment(v, e)                       => lookupVar(v.name) match
                                                   {
                                                     case Some(env) => val r = evaluate(e)
                                                                       if(typeAgree(env.memory.getType(v.name), r))
                                                                         env.memory(v.name) = r
                                                                       else
                                                                         sys.error(s"type mismatch at $statement")
                                                     case None      => sys.error(s"$v is used without declaration")
                                                   }
    case ProcedureCall(f)                       => evaluate(f)

    case If(e:BooleanValue, b)                  => if(e.value) execute(b)
    case IfElse(e:BooleanValue, tBlock, fBlock) => if(e.value)
                                                     execute(tBlock)
                                                   else
                                                     execute(fBlock)
    case If(e, b)                               => val v = evaluate(e)
                                                   execute(If(v, b))

    case IfElse(e, tBlock, fBlock)              => val v = evaluate(e)
                                                   execute(IfElse(v, tBlock, fBlock))

    case While(e, b)                            => evaluate(e) match
                                                   {
                                                     case BooleanValue(cond) => if(cond)
                                                                                {
                                                                                  execute(b)
                                                                                  execute(While(e, b))
                                                                                }
                                                     case _                  => sys.error(s"syntax error at $statement")
                                                   }

    case Print(es)                               => es.foreach{e => print(s"${evaluate(e)} ")}
                                                    println()
    case _                                       => sys.error(s"syntax error at $statement")
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
    case Equal()        => BooleanValue(left.value == right.value)
    case NotEqual()     => BooleanValue(left.value != right.value)
    case And()          => BooleanValue(left.value && right.value)
    case Or()           => BooleanValue(left.value || right.value)
    case _              => sys.error("NOT is not a binary op")
  }

  def invokeFunction(name:String, params:Seq[Value]):Value =
  {
    (name, params) match
    {
      case ("math.cos", NumericValue(v) :: Nil)                      => NumericValue(math.cos(v).toFloat)
      case ("math.sin", NumericValue(v) :: Nil)                      => NumericValue(math.sin(v).toFloat)
      case ("math.tan", NumericValue(v) :: Nil)                      => NumericValue(math.tan(v).toFloat)
      case ("math.sqrt", NumericValue(v) :: Nil)                     => NumericValue(math.sqrt(v).toFloat)
      case ("math.power", NumericValue(v) :: NumericValue(p) :: Nil) => NumericValue(math.pow(v, p).toFloat)

      case _          => findFunction(name, params, program) match
                         {
                           case Some(function) => invokeFunction(function, params)
                           case None           => sys.error(s"invokeFunction errors, there is no function $name that accept $params")
                         }
    }
  }

  def invokeFunction(function:FunctionDef, binding:Seq[Value]):Value =
  {
    // assume check has been made for functionDef args and binding
    push()
    for((v, b) <- function.signature.args zip binding)
    {
      top.memory.register(v.name, v.ptype)
      top.memory(v.name) = b
    }
    top.memory.register(function.signature.name, function.signature.rtype)
    function.body.statements.foreach{s => execute(s)}
    val rval = top.memory(function.signature.name) // return value
    pop()
    rval
  }

}

