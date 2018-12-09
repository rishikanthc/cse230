#!/usr/bin/env python3
from misc import Failure

class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__f=f
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0

class traced(object):
    '''
    This is the implementation of the traced decorator, which prints the function, its
    arguments and it's return value in a specific pattern. Eg.
    ,- foo(4, 5)
    | ,- foo(b=3, a=4)
    | | ,- foo(b=3, a=2)
    | | | ,- foo(b=1, a=2)
    | | | | ,- foo(b=1, a=0)
    | | | | `- 1
    | | | `- 1
    | | `- 1
    | `- 1
    `- 1
    1
    '''
    count = 0
    def __init__(self,f):
        '''Constructor which takes as argument the function to be traced and initializes
        its internal func variable with this function
        '''
        # replace this and fill in the rest of the class
        self.__name__ = f.__name__
        self.func = f

    def __call__(self, *args, **kargs):
        '''
        The call method which is responsible for calling the original function stored in
        internal variable func. It also processes the pattern to be printed before and
        after running the original function.
        '''
        printstr = "| " * traced.count
        traced.count = traced.count + 1
        if args:
            argval = ", ".join([str(val) for val in args])
        if kargs:
            argval = ", ".join([key + "=" + str(val) for key,val in kargs.items()])
        print(printstr + ",- " + self.__name__ + "(" + argval  + ")")
        try:
            rv = self.func(*args, **kargs)
        except Exception as e:
            rv = e
            traced.count = traced.count - 1
            raise rv
        
        traced.count = traced.count - 1
        print(printstr + "`- " + str(rv))
        return rv

class memoized(object):
    '''
    Implementation of the memoized decorator, which takes a function and stores its
    previous runs arguments and result so that it if it's run again, it can immediately
    return the result without re running the function.
    '''
    def __init__(self,f):
        '''
        Constructor which takes as argument the original function and stores it in its
        internal variable func. It also initializes a list prevRuns for the entire class, 
        which is used for storing past runs of the function
        '''
        # replace this and fill in the rest of the class
        self.__name__= f.__name__
        self.func = f
        self.prevRuns = []

    def __call__(self, *args, **kargs):
        '''
        The call method which executes the actual function(stored in internal variable
        func) and this method also stores the arguments and the return value of the 
        function in it's internal list variable prevRuns. On subsequent calls, it checks
        if the function has been run with the same arguments before and if it has,
        it returns the stored value, without re running the function. If not, it runs
        the function and stores its results along with its arguments in prevRuns.
        '''
        if args:
            argval = args
        if kargs:
            argval = tuple(kargs.values())

        
        rv = None
        for val in self.prevRuns:
            if argval == val[0]:
                rv = val[1]
        
        if rv == None:
            try:
                rv = self.func(*args, **kargs)
            except Exception as e:
                rv = e
            self.prevRuns.append((argval, rv))
        
        if isinstance(rv, Exception):
            raise rv
        else:
            return rv

# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print("RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a])))
        rv=f(*a)
        print("RETURNED %s" % repr(rv))

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)

