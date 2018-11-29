import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    min_diff = float('inf')
    

    if l == []:
        return None

    closest = l[0]

    for val in l:
        diff = abs(val - v)
        if diff  < min_diff:
            min_diff = diff
            closest = val

    return closest

def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""

    retVal = {}
    for k, v in zip(keys,values):
        retVal[k] = v

    return retVal

# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""

    wordCount = {}

    with open(fn) as fp:
        for line in fp:
            words = re.split('[^a-z0-9_]', line.lower())
            words = filter(None, words)
            for word in words:
                if word in wordCount.keys():
                    wordCount[word] += 1
                else:
                    wordCount[word] = 1

    return wordCount
