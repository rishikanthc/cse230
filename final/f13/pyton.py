#Q3.a
def lookup(d, k):
    return [val for key, val in d if key == k]

#Q3. helper
def cond(b, t, f):
    if b: return t
    else: return f

#Q3.b
def update(d, k, v):
    return [cond(k==key, (k, v), (key, val)) for key, val in d]

#Q3.c
def delete(d,k):
    return [(key, val) for key, val in d if key != k]

#Q3.d
def add(d, k, v):
    return d + [(k,v)]

#Q3.e
def update2(d, k, v):
    retVal = []

    for key, val in d:
        if key == k:
            retVal.append((key,v))
        else:
            retVal.append((key, val))
    
    return retVal

#Q4

def in_range(i, rv):
    def deco(f):
        def g(*args, **kargs):
            if g.i != -1:
                if args:
                    if args[g.i] > rv[1]:
                        raise Exception(str(g.i) + "th arg " + str(args[g.i]) +\
                                " too big")
                    elif args[g.i] < rv[0]:
                        raise Exception(str(g.i) + "th arg " + str(args[g.i]) +\
                                " too small")
                elif kargs:
                    val = kargs.values()
                    if val[g.i] > rv[1]:
                        raise Exception(str(g.i) + "th arg " + str(val[g.i]) +\
                                " too big")
                    elif val[g.i] < rv[0]:
                        raise Exception(str(g.i) + "th arg " + str(val[g.i]) +\
                                " too small")
            retVal = f(*args, **kargs)

            if g.i == -1:
                if retVal < rv[0]:
                    raise Exception("Return value " + str(retVal) + " too small")
                elif retVal > rv[1]:
                    raise Exception("Return value " + str(retVal) + " too big")
            return retVal
        g.i = i
        g.rv = rv
        return g
    return deco
