from misc import Failure

class Vector(object):
    vec = []
    def __init__(self, x):
        if issubclass(type(x), int):
            if x < 0 :
                raise ValueError("vector length cannot be negative")
            self.vec= [0.0] * x
            
        elif issubclass(type(x), list):
            self.vec= x
        else:
            raise TypeError("Invalid type")

    def __repr__(self):
        return "Vector(" + str(self.vec) + ")"
 
    def __len__(self):
        return len(self.vec)

    def __iter__(self):
        for x in self.vec:
            yield x

    def __add__(self, other):
        if issubclass(type(other), Vector):
            new_vec = [x+y for x,y in zip(self.vec, other.vec)]
        elif issubclass(type(other), list):
            new_vec = [x+y for x,y in zip(self.vec, other)]
        elif issubclass(type(other), tuple):
            new_vec = [x+y for (x,y) in zip(self.vec, other)]
        else:
            raise TypeError("invalid type")
        return Vector(new_vec)

    def __iadd__(self, other):
        if issubclass(type(other), Vector):
            self.vec = [x+y for x,y in zip(self.vec, other.vec)]
        elif issubclass(type(other), list):
            self.vec = [x+y for x,y in zip(self.vec, other)]
        elif issubclass(type(other), tuple):
            self.vec = [x+y for x,y in zip(self.vec, other)]
        else:
            raise TypeError("invalid type")

        return self

    def dot(self, other):
        other_array = []
        dotsum = 0

        if issubclass(type(other), Vector):
            other_array = other.vec
        elif issubclass(type(other), list):
            other_array = other
        elif issubclass(type(other), tuple):
            other_array = other
        else:
            raise TypeError("invalid type")

        for x,y in zip(self.vec, other_array):
            dotsum += (x*y)

        return dotsum

    def __getitem__(self, idx):
        if issubclass(type(idx), int):
            if idx < len(self.vec):
                return self.vec[idx]
            else:
                raise IndexError("Index out of range")
        else:
            raise TypeError("Index should be an integer")

    def __setitem__(self, idx, value):
        if issubclass(type(idx), int):
            if idx < len(self.vec):
                self.vec[idx] = value
            else:
                raise IndexError("Index out of range")
        else:
            raise TypeError("Index should be an integer")
