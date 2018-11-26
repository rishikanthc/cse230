from misc import Failure

class Vector(object):
    vec = []
    def __init__(self, x):
        if isinstance(x, int):
            if x < 0 :
                raise ValueError("vector length cannot be negative")
            self.vec= [0.0] * x
            
        elif isinstance(x, list):
            self.vec= x
        else:
            raise TypeError("Invalid type")
    
    def __repr__(self):
        return "Vector(" + str(self.vec) + ")"
        
