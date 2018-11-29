from misc import Failure

class Vector(object):
    vec = []
    def __init__(self, x):
        '''
        Constructor which initializes the vector depending on the type of the argument.
        If it's an int it creates a vector of same length.
        If the input is a list or a tuple it initializes the vector to the sequence passed
        '''
        if issubclass(type(x), int):
            if x < 0 :
                raise ValueError("vector length cannot be negative")
            self.vec= [0.0] * x
        elif issubclass(type(x), list):
            self.vec= x
        elif issubclass(type(x), tuple):
            self.vec = list(x)
        elif issubclass(type(x), str):
            self.vec = list(x)
        else:
            raise TypeError("Invalid type")

    def __repr__(self):
        '''
        This method implements a representation which prints the vector representation
        '''
        return "Vector(" + str(self.vec) + ")"

    def __len__(self):
        '''
        this method implements the functionality to find the length of the vector
        '''
        return len(self.vec)

    def __iter__(self):
        '''
        This method returns an iterable for the vector which allows for iterating over
        the vector
        '''
        for x in self.vec:
            yield x

    def __add__(self, other):
        '''
        Defines the + operator for vector types.
        Handles the cases of Vector vector addition
        Also handles the cases of Vector addition with a sequence.
        if the vectors are of different lengths then it raises an exception.
        if the operands are not of type vector or sequence then it raises an exception
        '''
        if issubclass(type(other), Vector):
            if len(self.vec) != len(other.vec):
                raise TypeError("invalid length")
            new_vec = [x+y for x,y in zip(self.vec, other.vec)]
        elif issubclass(type(other), list):
            if len(self.vec) != len(other):
                raise TypeError("invalid length")
            new_vec = [x+y for x,y in zip(self.vec, other)]
        elif issubclass(type(other), tuple):
            if len(self.vec) != len(other):
                raise TypeError("invalid length")
            new_vec = [x+y for (x,y) in zip(self.vec, other)]
        else:
            raise TypeError("invalid type")
        return Vector(new_vec)

    def __radd__(self, other):
        '''
        radd is used for reverse add, where in if the operand passed is of a different type,
        then the addition function reverses the arguments and sends it to the original
        add function
        '''
        return self.__add__(other)

    def __iadd__(self, other):
        '''
        implements the incrememntal add, wherein the operator += is implemented. 
        '''
        if issubclass(type(other), Vector):
            if len(self.vec) != len(other.vec):
                raise TypeError("invalid length")
            self.vec = [x+y for x,y in zip(self.vec, other.vec)]
        elif issubclass(type(other), list):
            if len(self.vec) != len(other):
                raise TypeError("invalid length")
            self.vec = [x+y for x,y in zip(self.vec, other)]
        elif issubclass(type(other), tuple):
            if len(self.vec) != len(other):
                raise TypeError("invalid length")
            self.vec = [x+y for x,y in zip(self.vec, other)]
        else:
            raise TypeError("invalid type")

        return self

    def dot(self, other):
        '''
        This method implements a dot product functionality for vectors or sequences.
        Sum of products
        '''
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
        '''
        this method implements the functionality to allow element wise access to the vector
        it also handles slices similar to that of lists
        '''
        if issubclass(type(idx), int):
            if idx < len(self.vec):
                return self.vec[idx]
            else:
                raise IndexError("Index out of range")
        elif issubclass(type(idx), slice):
            return self.vec[idx.start : idx.stop : idx.step]
        else:
            raise TypeError("Index should be an integer")

    def __setitem__(self, idx, value):
        '''
        This method allows for modifying or setting specific elements in the vector. It 
        takes as input the index and the value to write and modifies the vector with the same.
        It can also handle slices in a similar manner to that of lists.
        While handling slices, it checks that the operation does not modify the list of the
        vector, and if it does it throws an expection and does not perform the operation.
        '''
        typeval = type(idx)
        vecLen = len(self.vec)

        if issubclass(typeval, int):
            if idx < len(self.vec):
                self.vec[idx] = value
            else:
                raise IndexError("Index out of range")
        elif issubclass(typeval, slice):

            start_idx = 0 if idx.start == None else idx.start
            end_idx = len(self.vec) if idx.stop == None else idx.stop
            step_idx = 1 if idx.step == None else idx.step

            if len(range(start_idx, end_idx, step_idx)) != len(value):
                raise ValueError("length cannot be modified")
            self.vec[idx.start : idx.stop : idx.step] = value
        else:
            raise TypeError("Index should be an integer")

    def __eq__(self, other):
        '''
        Implements the equality comparison for vectors. It does this by, checking element
        wise for all elements in the 2 vectors passed. If an operand of type other than
        vector is passed, then it raises an exception
        '''
        if issubclass(type(other), Vector):
            cmpr = [x == y for x,y in zip(self.vec, other.vec)]
            if sum(cmpr) == len(self.vec):
                return True
            else:
                return False
        else:
            return False

    def __ne__(self, other):
        '''
        Implements the non equality comparison for vectors, if the vectors are not of 
        appropriate types, it raises an exception
        '''
        if issubclass(type(other), Vector):
            cmpr = [x == y for x,y in zip(self.vec, other.vec)]
            if sum(cmpr) != len(self.vec):
                return True
            else:
                return False
        else:
            return True

    def __lt__(self, other):
        '''
        It implements the less than comparison for vectors. It does this by checking the
        highest element of both vectors. If the self value is lesser than that of the other arg
        passed, then it returns true and otherwise checks the next highest value.
        If all values are equal then it returns false
        '''
        self_vec = sorted(self.vec, reverse = True)
        other_vec = sorted(other.vec, reverse = True)

        for x,y in zip(self_vec, other_vec):
            if x == y:
                continue
            return x < y

        return False

    def __gt__(self, other):
        '''
        Implements greater than - exactly the same as lt but inverted
        '''
        self_vec = sorted(self.vec, reverse = True)
        other_vec = sorted(other.vec, reverse = True)

        for x,y in zip(self_vec, other_vec):
            if x == y:
                continue
            return x > y

        return False

    def __le__(self, other):
        '''
        Implements less than equal - similar to lt but handles equality as well
        '''
        self_vec = sorted(self.vec, reverse = True)
        other_vec = sorted(other.vec, reverse = True)

        for x,y in zip(self_vec, other_vec):
            if x == y:
                continue
            return x < y

        return True

    def __ge__(self, other):
        '''
        Implements greater than equal - similar to gt but handles equality as well
        '''
        self_vec = sorted(self.vec, reverse = True)
        other_vec = sorted(other.vec, reverse = True)

        for x,y in zip(self_vec, other_vec):
            if x == y:
                continue
            return x > y

        return True
