

class StrCon:
    def __init__(self, ctag, cons1=None, cons2=None):
        self.ctag = ctag
        self.cons1 = cons1
        self.cons2 = cons2

StrNil = StrCon(0)

def generator_of_stream(fxs):
    while True:
        cxs = fxs()
        if cxs.ctag == 0:
            break
        else:
            fxs = cxs.cons2
            yield cxs.cons1
    raise StopIteration

def theNatPairs():
    def pairs(i=0, j=0):
        while True:
            if i==0 and j ==0:
                yield StrCon(1, (i, j), lambda: next(pairs(0, 1)))
         
            elif i == 0:
                yield StrCon(1, (i, j), lambda: next(pairs( i+1, j-1)))
             
                
            elif j == 0:
                  yield StrCon(1, (i, j), lambda: next(pairs(0 , i+1)))
     
            
            else:
                yield StrCon(1, (i, j), lambda: next(pairs(i +1, j - 1)))


    return generator_of_stream(lambda: next(pairs()))