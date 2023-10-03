########################################################################
# Assign3:
# //
# DUE: the 4th of October, 2023
# //
# Total: 80 points + 30 bonus points
# (OCaml: 50+30 points)(Python: 30 points)
########################################################################
# Higher-order programming in Python
########################################################################
#
# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
#
########################################################################

############## end of [CS320-2023-Fall-assigns-assign3.py] ##############


class mylist_nil:
    def __repr__(self):
        return "MyNil"

class mylist_cons:
    def __init__(self, x, xs):
        self.x = x
        self.xs = xs

    def __repr__(self):
        return f"MyCons({self.x}, {self.xs})"

class mylist_snoc:
    def __init__(self, xs, x):
        self.xs = xs
        self.x = x

    def __repr__(self):
        return f"MySnoc({self.xs}, {self.x})"

class mylist_reverse:
    def __init__(self, xs):
        self.xs = xs

    def __repr__(self):
        return f"MyReverse({self.xs})"

class mylist_append2:
    def __init__(self, xs1, xs2):
        self.xs1 = xs1
        self.xs2 = xs2

    def __repr__(self):
        return f"MyAppend2({self.xs1}, {self.xs2})"








        
def mylist_foreach(xs, work):
    if isinstance(xs, mylist_nil):
        return 
    elif isinstance(xs, mylist_cons):
        work(xs.x)  
        mylist_foreach(xs.xs, work)  
    elif isinstance(xs, mylist_snoc):
        mylist_foreach(xs.xs, work)  
        work(xs.x)  
    elif isinstance(xs, mylist_reverse):
       
          mylist_foreach(xs.xs, work)
    elif isinstance(xs, mylist_append2):
        mylist_foreach(xs.xs1, work)
        mylist_foreach(xs.xs2, work)




def mylist_rforeach(xs, work):
    if isinstance(xs, mylist_nil):
        return
    elif isinstance(xs, mylist_cons):
        mylist_rforeach(xs.xs, work)
        work(xs.x)
    elif isinstance(xs, mylist_reverse):
        mylist_rforeach(xs.xs, work)
    elif isinstance(xs, mylist_snoc):
        work(xs.x)
        mylist_rforeach(xs.xs, work)
    elif isinstance(xs, mylist_append2):
        mylist_rforeach(xs.xs2, work)
        mylist_rforeach(xs.xs1, work)
        
def foreach_to_map_pylist(foreach):
    def map_pylist(xs, fopr_func):
        res = []
        def work_func(x0):
            nonlocal res
            res.append(fopr_func(x0))
            return None
        foreach(xs, work_func)
        return res
    return map_pylist


