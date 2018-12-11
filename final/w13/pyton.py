def transpose(m):
    height = len(m)
    width = len(m[0])

    return [[m[j][i] for j in range(height)] for i in range(width)]

g = [[ 0, 0, 0, 0, 0 ],[ 0, 0, 0, 0, 0 ],[ 0, 1, 1, 1, 0 ],[ 0, 0, 0, 0, 0 ],
        [ 0, 0, 0, 0, 0 ]]

def access(g, x, y):
    try: return g[y][x]
    except : return 0

def count_live_neighbours(g, x, y):
    live = 0
    for x_delta in [x-1, x, x+1]:
        for y_delta in [y-1, y, y+1]:
            if access(g, x_delta, y_delta):
                live += 1

    return live - access(g,x,y)

def new_val(g, x, y):
    live = count_live_neighbours(g, x, y)
    curr = g[y][x]

    if live < 2 or live > 3:
        return 0
    elif live == 3:
        return 1
    else:
        return curr

def step(g):
    height = len(g)
    width = len(g[0])
    
    return [[new_val(g, x, y) for x in range(width)] for y in range(height)]

def lift_1(f):
    def decorated(x):
        return [f(val) for val in x]
    return decorated

def lift_2(f):
    def decorated(x,y):
        return [f(x[i],y[i]) for i in range(len(x))]
    return decorated

def lift(f):
    def decorated(*args):
        return [f(*val) for val in transpose(args)]
    return decorated

