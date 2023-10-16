class Strcon:
    def __init__(self, value):
        self.value = value

class Stream:
    def __init__(self):
        self.current = None

    def next(self):
        if self.current is None:
            return None
        else:
            result = self.current.value
            self.current = self.current.value()
            return result

def theNatPairs():
    def pairs(i, j):
        while True:
            yield (i, j)
            if i == 0:
                i, j = i + 1, j - 1
            elif j == 0:
                i, j = 0, i + 1
            else:
                i, j = i + 1, j - 1

    stream = Stream()
    stream.current = Strcon((0, 0))
    stream.current.value = lambda: Strcon((0, 0)).value

    return pairs(0, 1)
