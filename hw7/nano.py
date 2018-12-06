#!/usr/bin/env python3

from misc import Failure

# prologable interface
class Prologable():
    def toProlog(self) -> str:
        raise Failure("SHOULD NOT GET HERE -- subclasses should override")

    def __eq__(self, other):
        if isinstance(other, Prologable):
            return self.toProlog() == other.toProlog()
        else:
            return False
    def __str__(self):
        return self.toProlog()


# expression interface
class Expression(Prologable):
    def toProlog(self):
        if isinstance(self, Bin):
            return 'bin(' + self.l.toProlog() +  ', ' + self.o.toProlog() + ', ' + \
                    self.r.toProlog() + ')'
        elif isinstance(self, Var):
            return 'var(' + self.v + ')'
        elif isinstance(self, Const):
            return 'const(' + str(self.v) + ')'
        elif isinstance(self, Bool):
            return 'boolean(' + str(self.b) + ')'
        elif isinstance(self, NilExpr):
            return 'nil'
        elif isinstance(self, If):
            return 'ite(' + self.c.toProlog() + ', ' + self.t.toProlog() + ', ' + \
                    self.f.toProlog() + ')'
        elif isinstance(self, Let):
            return 'let(' + self.v + ', ' + self.e.toProlog() + ', ' + \
                    self.body.toProlog() + ')'
        elif isinstance(self, Letrec):
            return 'letrec(' + self.v + ', ' + self.e.toProlog() + ', ' + \
                    self.body.toProlog() + ')'
        elif isinstance(self, App):
            return 'app(' + self.f.toProlog() + ', ' + self.arg.toProlog() + ')'
        elif isinstance(self, Fun):
            return 'fun(' + self.v + ', ' + self.body.toProlog() + ')'

# binop interface
class Bop(Prologable):
    def toProlog(self):
        if isinstance(self, Plus):
            return 'plus'
        elif isinstance(self, Minus):
            return 'minus'
        elif isinstance(self, Mul):
            return 'mul'
        elif isinstance(self, Div):
            return 'div'
        elif isinstance(self, Eq):
            return 'eq'
        elif isinstance(self, Neq):
            return 'neq'
        elif isinstance(self, Lt):
            return 'lt'
        elif isinstance(self, Leq):
            return 'leq'
        elif isinstance(self, And):
            return 'and'
        elif isinstance(self, Or):
            return 'or'
        elif isinstance(self, Cons):
            return 'cons'

class Plus(Bop):
    pass
class Minus(Bop):
    pass
class Mul(Bop):
    pass
class Div(Bop):
    pass
class Eq(Bop):
    pass
class Neq(Bop):
    pass
class Lt(Bop):
    pass
class Leq(Bop):
    pass
class And(Bop):
    pass
class Or(Bop):
    pass
class Cons(Bop):
    pass

# Expressions
class Const(Expression):
    def __init__(self, i: int):
        self.v = i
class Bool(Expression):
    def __init__(self, b: bool):
        self.v = b
class NilExpr(Expression):
    def __init__(self):
        return
class Var(Expression):
    def __init__(self, v: str):
        self.v = v
    
class Bin(Expression):
    def __init__(self, l: Expression, o: Bop, r:Expression):
        self.l = l
        self.r = r
        self.o = o
    
class If(Expression):
    def __init__(self, c: Expression, t: Expression, f: Expression):
        self.c = c
        self.t = t
        self.f = f

class Let(Expression):
    def __init__(self, v: str, e: Expression, body: Expression):
        self.v = v
        self.e = e
        self.body = body

class Letrec(Expression):
    def __init__(self, v: str, e: Expression, body: Expression):
        self.v = v
        self.e = e
        self.body = body

class App(Expression):
    def __init__(self, f: Expression, arg: Expression):
        self.f = f
        self.arg = arg

class Fun(Expression):
    def __init__(self, v: str, body: Expression):
        self.v = v
        self.body = body


# Types

class Type(Prologable):
    def toProlog(self):
        if isinstance(self, IntTy):
            return 'int'
        elif isinstance(self, BoolTy):
            return 'bool'
        elif isinstance(self, ArrowTy):
            return 'arrow(' + self.l.toProlog() +', ' + self.r.toProlog() + ')'
        elif isinstance(self, ListTy):
            return 'list(' + self.inner.toProlog() + ')'
        elif isisntance(self, VarTy):
            return 'var ' + self.name + ')'

class IntTy(Type):
    def __init__(self):
        return

class BoolTy(Type):
    def __init__(self):
        return

class ArrowTy(Type):
    def __init__(self, l: Type, r: Type):
        self.l = l
        self.r = r

class ListTy(Type):
    def __init__(self, inner: Type):
        self.inner = inner

class VarTy(Type):
    def __init__(self, name: str):
        self.name = name

