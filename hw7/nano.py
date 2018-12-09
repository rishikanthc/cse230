#!/usr/bin/env python3

from misc import Failure
# prologable interface
class Prologable():
    def toProlog(self) -> str: raise Failure("SHOULD NOT GET HERE -- subclasses should override")

    def __eq__(self, other):
        if isinstance(other, Prologable):
            return self.toProlog() == other.toProlog()
        else:
            return False
    def __str__(self):
        return self.toProlog()


# expression interface
class Expression(Prologable):
    '''
    This class implements functionality required to parse expressions and convert them to
    their equivalent prolog term. It implements the toProlog() function which is 
    inherited by the sub-classes. The toProlog() method, checks the type of the expression
    and based on the specific cases it maps to, performs type specific conversions.
    '''
    def toProlog(self) -> str: raise Failure("SHOULD NOT GET HERE -- subclasses should override")

# binop interface
class Bop(Prologable):
    '''
    This class is the base class representation of all binary operators: +,-,*,/,=,!=,<,
    <=,&,|,::. The class provides the method toProlog() which converts the binary 
    operators into their equivalent prolog types. This method is available to all 
    sub-classes which inherit this class.
    '''
    def toProlog(self) -> str: raise Failure("SHOULD NOT GET HERE -- subclasses should override")
    
class Plus(Bop):
    '''
    This class provides the representation sub class for the binary operation +.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        ''' Return plus for the Plus operator'''
        return 'plus'

class Minus(Bop):
    '''
    This class provides the representation sub class for the binary operation -.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        ''' Return minus for the Minus operator'''
        return 'minus'

class Mul(Bop):
    '''
    This class provides the representation sub class for the binary operation *.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        ''' Return mul for the Multiplication operator'''
        return 'mul'

class Div(Bop):
    '''
    This class provides the representation sub class for the binary operation /.
    The class inherits from the parent class Bop .
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        '''Return div for the Division operator'''
        return 'div'

class Eq(Bop):
    '''
    This class provides the representation sub class for the binary operation =.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        '''return eq for the equal operator'''
        return 'eq'

class Neq(Bop):
    '''
    This class provides the representation sub class for the binary operation !=.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        '''Returns neq for != operator'''
        return 'neq'

class Lt(Bop):
    '''
    This class provides the representation sub class for the binary operation <.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        ''' Returns lt for the < operator'''
        return 'lt'

class Leq(Bop):
    '''
    This class provides the representation sub class for the binary operation <=.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        '''Return leq for the <= operator'''
        return 'leq'

class And(Bop):
    '''
    This class provides the representation sub class for the binary operation &.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        '''Returns and for the and operator'''
        return 'and'

class Or(Bop):
    '''
    This class provides the representation sub class for the binary operation |.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        ''' Returns or for the or operator'''
        return 'or'

class Cons(Bop):
    '''
    This class provides the representation sub class for the binary operation ::.
    The class inherits from the parent class Bop.
    Hence this class has access to the toProlog() method.
    '''
    def toProlog(self):
        '''Returns cons for the Cons operator'''
        return 'cons'

# Expressions
class Const(Expression):
    '''
    This class provides the representation of constant expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, i: int):
        '''
        Constructor takes as argument the integer constant and initializes the value v
        of the object with the integer
        '''
        self.v = i
            
    def toProlog(self):
        '''Retruns const(value) for the const operator'''
        return 'const(' + str(self.v) + ')'

class Bool(Expression):
    '''
    This class provides the representation of Bool expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, b: bool):
        '''
        Constructor takes as argument a boolean value and initializes it's internal value
        v with the boolean value.
        '''
        self.v = b

    def toProlog(self):
        '''returns boolean(value) for the boolean operator'''
        return 'boolean(' + str(self.v) + ')'

class NilExpr(Expression):
    '''
    This class provides the representation of NilExpr expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self):
        return
    
    def toProlog(self):
        '''returns nil for NilExpr'''
        return 'nil'

class Var(Expression):
    '''
    This class provides the representation of Var expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, v: str):
        '''
        Constructor takes as argument a string for the variable name and initializes it's
        internal variable with it.
        '''
        self.v = v

    def toProlog(self):
        '''returns var(name) for the var type'''
        return 'var(' + self.v + ')'
 
class Bin(Expression):
    '''
    This class provides the representation of Bop expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, l: Expression, o: Bop, r:Expression):
        '''
        Constructors which takes as input the 2 operands of type expression and the binary
        operator and initializes it's internal variables l,o,r with the same.
        '''
        self.l = l
        self.r = r
        self.o = o
    
    def toProlog(self):
        '''Returns the prolog representation of binary operators. Eg. Bin(const(1), plus
        ,const(2))'''
        return 'bin(' + self.l.toProlog() +  ', ' + self.o.toProlog() + ', ' + \
                    self.r.toProlog() + ')'
    
class If(Expression):
    '''
    This class provides the representation of If expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, c: Expression, t: Expression, f: Expression):
        '''
        Constructor takes as argument the condition, the true expression and the false
        expression and initalizes it's internal variables c,t,f with the same.
        '''
        self.c = c
        self.t = t
        self.f = f

    def toProlog(self):
        '''Returns the corresponding representation of the If statement'''
        return 'ite(' + self.c.toProlog() + ', ' + self.t.toProlog() + ', ' + \
                    self.f.toProlog() + ')'

class Let(Expression):
    '''
    This class provides the representation of Let expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, v: str, e: Expression, body: Expression):
        '''
        Constructor which takes as argument the name, the expression and the body and
        initializes it's internal variables v,e,body with the same.
        '''
        self.v = v
        self.e = e
        self.body = body
    
    def toProlog(self):
        '''Returns the representation of let expressions'''
        return 'let(' + self.v + ', ' + self.e.toProlog() + ', ' + \
                    self.body.toProlog() + ')'

class Letrec(Expression):
    '''
    This class provides the representation of Letrec expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, v: str, e: Expression, body: Expression):
        '''
        Constructor which takes as argument the name, the expression and the body and
        initializes it's internal variables v,e,body with the same.
        '''
        self.v = v
        self.e = e
        self.body = body

    def toProlog(self):
        '''Returns the representation of letrec expressions'''
        return 'letrec(' + self.v + ', ' + self.e.toProlog() + ', ' + \
                    self.body.toProlog() + ')'

class App(Expression):
    '''
    This class provides the representation of App expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, f: Expression, arg: Expression):
        '''
        constructor which takes as argument the function expression and the argument 
        expression and initializes its internal variables f, arg with the same.
        '''
        self.f = f
        self.arg = arg
    
    def toProlog(self):
        '''returns the representatin of app for App '''
        return 'app(' + self.f.toProlog() + ', ' + self.arg.toProlog() + ')'

class Fun(Expression):
    '''
    This class provides the representation of Fun expressions. It inherits the
    Expression class and correspondingly uses the toProlog() method.
    '''
    def __init__(self, v: str, body: Expression):
        '''
        Constructor which takes as argument the name and the function body, and 
        initializes its internal variables  v, body with the same.
        '''
        self.v = v
        self.body = body

    def toProlog(self):
        '''returns function representation'''
        return 'fun(' + self.v + ', ' + self.body.toProlog() + ')'

# Types

class Type(Prologable):
    def toProlog(self) -> str: raise Failure("SHOULD NOT GET HERE -- subclasses should override")
    
class IntTy(Type):
    '''
    Class which represents the int type. 
    '''
    def __init__(self):
        return

    def toProlog(self):
        '''
        Returns prolog type int for Int types 
        '''
        return 'int'

class BoolTy(Type):
    def __init__(self):
        return

    def toProlog(self):
        ''' Return bool for boolean type'''
        return 'bool'

class ArrowTy(Type):
    '''
    Class which represents the type of function: eg. int -> int 
    '''
    def __init__(self, l: Type, r: Type):
        '''
        Constructor takes as argument the argument type and the return type and initalizes
        it's internal variable l,r with them.
        '''
        self.l = l
        self.r = r

    def toProlog(self):
        '''Converts function type into arrow(type1, type2)'''
        return 'arrow(' + self.l.toProlog() +', ' + self.r.toProlog() + ')'
    
class ListTy(Type):
    '''
    Class which represents the list type
    '''
    def __init__(self, inner: Type):
        '''
        Constructor which takes as argument the type of the list and initalizes its
        internal variable inner with the same.
        '''
        self.inner = inner

    def toProlog(self):
        '''Converts ListTy to prolog representation list()'''
        return 'list(' + self.inner.toProlog() + ')'

class VarTy(Type):
    '''
    Class which represent ths var type.
    '''
    def __init__(self, name: str):
        '''
        Constructor takes as argument the name of the variable and initializes its
        internal variable name with it.
        '''
        self.name = name

    def toProlog(self):
        ''' converts var type to prolog representation var name'''
        return self.name

