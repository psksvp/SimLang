//
//  main.swift
//  CaseClass
//
//  Created by psksvp on 25/11/17.
//  Copyright © 2017 psksvp. All rights reserved.
//

protocol ASTNode
{}

enum Operator :ASTNode
{
  case Plus
  case Minus
  case Multiply
  case Division
  case Modulo
  
  case Equal
  case NotEqual
  case Greater
  case GreaterOrEqual
  case Less
  case LessOrEqual
  
  case And
  case Or
  case Not
}

indirect enum Type :ASTNode
{
  case Numeric
  case Boolean
  case Text
  case Array(type:Type)
}

enum Value :ASTNode
{
  case Numeric(Float)
  case Boolean(Bool)
  case Text(String)
}

indirect enum Expr :ASTNode
{
  case Literal(rep:String)
  case Variable(name:String)
  case Binary(left:Expr, op:Operator, right:Expr)
  case Unary(op:Operator, expr:Expr)
}

indirect enum Statement :ASTNode
{
  case Block(body:[Statement])
  case Declaration(variable:Expr, type:Type)
  case DeclarationAndAssignment(variable:Expr, type:Type, expr:Expr)
  case Assignment(variable:Expr, expr:Expr) // need to check left is variable
  case If(cond:Expr, body:Statement)
  case IfElse(cond:Expr, trueBody:Statement, falseBody:Statement)
  case While(cond:Expr, body:Statement)
  case Print(expr:Expr)
}

class Program :ASTNode 
{
  let name:String
  let block:Statement
  
  init(name:String, block:Statement)
  {
    self.name = name
    self.block = block
  }
}



class Machine
{
  var memory = [String:Value]()
  var register = [String:Type]()
  
  init()
  {
  }
  
  func run(program:Program) -> ([String:Type], [String:Value])
  {
    execute(statement:program.block)
    return (register, memory)
  }

  
  func execute(statement:Statement) -> Void
  {
    switch statement
    { 
      case let .Block(statements)                 : for s in statements
                                                    {
                                                      execute(statement:s)
                                                    }                       
      case let .Declaration(v, t)                 : declare(variable:v, withType:t)
      case let .DeclarationAndAssignment(v, t, e) : declare(variable:v, withType:t)
                                                    set(variable:v, toValue:execute(expr:e))
      
      case let .Assignment(v, e)                  : set(variable:v, toValue:execute(expr:e)) 
      case let .If(e, b)                          : switch execute(expr:e)
                                                    {
                                                      case let .Boolean(ve) where true == ve  : execute(statement:b)
                                                      case let .Boolean(ve) where false == ve : () //skip()
                                                      default                                 : print("TODO pump error1")
                                                    }
      case let .IfElse(e, tb, fb)                 : switch execute(expr:e)
                                                    {
                                                      case let .Boolean(ve) where true == ve  : execute(statement:tb)
                                                      case let .Boolean(ve) where false == ve : execute(statement:fb)
                                                      default                                 : print("TODO pump error")
                                                    } 
      case let .While(e, b)                       : switch execute(expr:e)
                                                    {
                                                      case let .Boolean(ve) where true == ve  : execute(statement:b)
                                                                                                execute(statement:Statement.While(cond:e, body:b))
                                                      case let .Boolean(ve) where false == ve : () //skip()
                                                      default               : print("TODO pump error e is not a boolean")
                                                    }  
      case let .Print(e)                          : print(execute(expr:e))                                                               
    }
  }
  
  
  func execute(expr:Expr) -> Value
  {
    switch expr
    {
      case let .Literal(s)      : return string2Value(litString:s)
      case let .Variable(s)     : return memory[s]!               // TODO: need to check for undefined var
      case let .Binary(l, o, r) : let lv = execute(expr:l)
                                  let lr = execute(expr:r)
                                  return computeBinaryOp(left:lv, op:o, right:lr)
      case let .Unary(o, e)     : let le = execute(expr:e)
                                  return computeUnaryOp(op:o, opd:le)                       
                                  
    }
  } 
  
  func set(variable:Expr, toValue:Value) -> Void
  {
    // TODO: need to do a type check between variable type and toValue
    switch variable
    {
      case let .Variable(n) : if nil != register[n]
                              {
                                 memory[n] = toValue   // TODO pump error on else
                              }
      default               : print("Error") // TODO: need to pump error, v is not a variable 
    }                      
  }
  
  func declare(variable:Expr, withType:Type) -> Void
  {
    switch variable
    {
      case let .Variable(n) : if nil == register[n]
                              {
                                 register[n] = withType   // TODO pump error on else
                              }
                          
      default               : print("Error") // TODO: need to pump error, v is not a variable
    }
  }
  
  func string2Value(litString:String) -> Value
  {
    if nil != Float(litString)
    {
      return Value.Numeric(Float(litString)!)
    }
    else if nil != Bool(litString)
    {
      return Value.Boolean(Bool(litString)!)
    }
    else
    {
      return Value.Text(litString)
    }
    
  }
  
  func computeBinaryOp(left:Value, op:Operator, right:Value) -> Value
  {
    switch (left, op, right) 
    {
      // numeric
      case let (.Numeric(a), .Plus, .Numeric(b))           : return Value.Numeric(a + b)
      case let (.Numeric(a), .Minus, .Numeric(b))          : return Value.Numeric(a - b)
      case let (.Numeric(a), .Multiply, .Numeric(b))       : return Value.Numeric(a * b)
      case let (.Numeric(a), .Division, .Numeric(b))       : return Value.Numeric(a / b) // div by zero
      case let (.Numeric(a), .Modulo, .Numeric(b))         : return Value.Numeric(Float(Int(a) % Int(b))) // div by zero
      // relation
      case let (.Numeric(a), .Equal, .Numeric(b))          : return Value.Boolean(a == b)
      case let (.Numeric(a), .NotEqual, .Numeric(b))       : return Value.Boolean(a != b)
      case let (.Numeric(a), .Greater, .Numeric(b))        : return Value.Boolean(a > b)
      case let (.Numeric(a), .GreaterOrEqual, .Numeric(b)) : return Value.Boolean(a >= b) 
      case let (.Numeric(a), .Less, .Numeric(b))           : return Value.Boolean(a < b)
      case let (.Numeric(a), .LessOrEqual, .Numeric(b))    : return Value.Boolean(a <= b) 
      //connective
      case let (.Boolean(a), .Equal, .Boolean(b))          : return Value.Boolean(a == b)
      case let (.Boolean(a), .NotEqual, .Boolean(b))       : return Value.Boolean(a != b)
      case let (.Boolean(a), .And, .Boolean(b))            : return Value.Boolean(a && b)
      case let (.Boolean(a), .Or, .Boolean(b))             : return Value.Boolean(a || b)
      default                                              : return Value.Numeric(-1) // just a dummy, pump error
    }
  }
  
  func computeUnaryOp(op:Operator, opd:Value) -> Value
  {
    switch(op, opd)
    {
      case let (.Minus, .Numeric(n)) : return Value.Numeric(-n)
      case let (.Plus, .Numeric(n))  : return Value.Numeric(+n)
      case let (.Not, .Boolean(n))   : return Value.Boolean(!n)
      default                        : return Value.Numeric(-1) // just a dummy, pump error                   
    }
  }
} // class Machine


func test()
{
  let m = Machine()
  
  let s = Statement.Block(body:[Statement.DeclarationAndAssignment(variable:Expr.Variable(name:"a"), 
                                                                       type:Type.Numeric, 
                                                                       expr:Expr.Literal(rep:"1")),
                                Statement.DeclarationAndAssignment(variable:Expr.Variable(name:"s"), 
                                                                       type:Type.Numeric, 
                                                                       expr:Expr.Literal(rep:"0")),                                       
                                Statement.While(cond:Expr.Binary(left:Expr.Variable(name:"a"),
                                                                   op:Operator.LessOrEqual, 
                                                                right:Expr.Literal(rep:"1000")),            
                                                body:Statement.Block(body:[Statement.IfElse(cond:Expr.Binary(left:Expr.Literal(rep:"0"), 
                                                                                                               op:Operator.NotEqual,
                                                                                                            right:Expr.Binary(left:Expr.Variable(name:"a"),
                                                                                                                                op:Operator.Modulo,
                                                                                                                             right:Expr.Literal(rep:"2"))),
                                                                                        trueBody:Statement.Print(expr:Expr.Literal(rep:"even")),
                                                                                       falseBody:Statement.Print(expr:Expr.Literal(rep:"odd"))),
                                                                           Statement.Assignment(variable:Expr.Variable(name:"s"), 
                                                                                                    expr:Expr.Binary(left:Expr.Variable(name:"s"),
                                                                                                                       op:Operator.Plus,
                                                                                                                    right:Expr.Variable(name:"a"))),
                                                                           Statement.Assignment(variable:Expr.Variable(name:"a"), 
                                                                                                    expr:Expr.Binary(left:Expr.Variable(name:"a"),
                                                                                                                       op:Operator.Plus,
                                                                                                                    right:Expr.Literal(rep:"1")))
                                                                   ]))                                       
                               ])
  let p = Program(name:"Hello", block:s)                        
  let r = m.run(program:p)  
  print(r)              
}




test()
