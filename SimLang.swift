//
//  main.swift
//  CaseClass
//
//  Created by psksvp on 25/11/17.
//  Copyright © 2017 psksvp. All rights reserved.
//

import Foundation

enum Operator
{
  case Plus
  case Minus
  case Multiply
  case Division
  case Modulo
  
  case Equal
  case Greater
  case GreaterOrEqual
  case Less
  case LessOrEqual
  
  case And
  case Or
  case Not
}

indirect enum Type
{
  case Numeric
  case Boolean
  case Text
  case Array(type:Type)
}

indirect enum Expr
{
  case Literal(rep:String)
  case Variable(name:String)
  case Binary(left:Expr, op:Operator, right:Expr)
  case Unary(op:Operator, expr:Expr)
}

indirect enum Statement
{
  case Declaration(variable:Expr, type:Type)
  case DeclarationAndAssignment(variable:Expr, type:Type, expr:Expr)
  case Assignment(variable:Expr, expr:Expr) // need to check left is variable
  case If(cond:Expr, body:[Statement])
  case IfElse(cond:Expr, trueBody:[Statement], falseBody:[Statement])
  case While(cond:Expr, body:[Statement])
}

enum Code
{
  case Program(name:String, statements:[Statement], args:[String])
}


class Machine
{
  enum Value
  {
    case Numeric(Float)
    case Boolean(Bool)
    case Text(String)
  }
  
  let memory = [String:Value]()
  let register = [String:Type]()
  
  init()
  {
  }
  
  func run(code:Code) -> Void
  {
  }
  
  /*
  func execute(statement:Statement) -> Void
  {
    switch statement
    {
      case let .Declaration(v, t) : register[v.name] = t
      //case let .Assignment(v, e). : 
    }
  } */
  
  
  func execute(expr:Expr) -> Value
  {
    /*
    switch expr
    {
      case let .Binary(l, o, r) : 
    } */
    return Value.Numeric(9.0)
  } 
  
  func boolean(expr:Expr) -> Bool
  {
    switch expr
    {
      case let .Binary(_, op, _) : return boolean(op: op)
      case let .Unary(op, _)     : return op == Operator.Not
      default                    : return false
    }
  }
  
  func boolean(op:Operator) -> Bool
  {
    return !(op == Operator.Plus ||
             op == Operator.Minus ||
             op == Operator.Multiply ||
             op == Operator.Division ||
             op == Operator.Modulo)
  }
}



func walk(expr:Expr)
{
  switch expr
  {
    case let .Literal(e)      : print("visit a lit " + e)
    case let .Variable(n)     : print("visit a var " + n)
    case let .Binary(l, o, r) : print("visit a binary")
                                walk(expr:l)
                                walk(expr:r)
    case let .Unary(o, e)     : print("visit a unary")
                                walk(expr:e)
  }
}

func test()
{
  let v = Expr.Unary(op: Operator.Minus,
                     expr: Expr.Variable(name: "g"))
  
  let e = Expr.Binary(left: Expr.Literal(rep: "1"),
                      op: Operator.Plus,
                      right: Expr.Variable(name: "h"))
  let e2 = Expr.Binary(left: v, op: Operator.Plus, right: e)
  
  
  print("----------------")
  walk(expr:e)
  print("----------------")
  walk(expr:e2)
}

test()
