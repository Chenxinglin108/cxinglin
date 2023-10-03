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


class MyNil:
    def __repr__(self):
        return "MyNil"

class MyCons:
    def __init__(self, x, xs):
        self.x = x
        self.xs = xs

    def __repr__(self):
        return f"MyCons({self.x}, {self.xs})"

class MySnoc:
    def __init__(self, xs, x):
        self.xs = xs
        self.x = x

    def __repr__(self):
        return f"MySnoc({self.xs}, {self.x})"

class MyReverse:
    def __init__(self, xs):
        self.xs = xs

    def __repr__(self):
        return f"MyReverse({self.xs})"

class MyAppend2:
    def __init__(self, xs1, xs2):
        self.xs1 = xs1
        self.xs2 = xs2

    def __repr__(self):
        return f"MyAppend2({self.xs1}, {self.xs2})"

# Example usage:

# Use repr_xs0, repr_xs1, etc. as needed

xs0 = MyNil()
xs1 = MyCons(1, xs0)
xs2 = MySnoc(xs0, 2)
xs3 = MyAppend2(xs1, xs2)
xs4 = MyAppend2(xs3, xs3)
xs5 = MyReverse(xs4)
xs6 = MyAppend2(xs5, xs5)
xs7 = MyAppend2(xs6, xs6)
        

def mylist_foreach(xs, work):
    if isinstance(xs, MyNil):
        return
    elif isinstance(xs, MyCons):
        work(xs.x)
        mylist_foreach(xs.xs, work)
    elif isinstance(xs, MyReverse):
        mylist_foreach(xs.xs, work)
    elif isinstance(xs, MySnoc):
        mylist_foreach(xs.xs, work)
        work(xs.x)
    elif isinstance(xs, MyAppend2):
        mylist_foreach(xs.xs1, work)
        mylist_foreach(xs.xs2, work)



def mylist_rforeach(xs, work):
    if isinstance(xs, MyNil):
        return
    elif isinstance(xs, MyCons):
        mylist_rforeach(xs.xs, work)
        work(xs.x)
    elif isinstance(xs, MyReverse):
        mylist_rforeach(xs.xs, work)
    elif isinstance(xs, MySnoc):
        work(xs.x)
        mylist_rforeach(xs.xs, work)
    elif isinstance(xs, MyAppend2):
        mylist_rforeach(xs.xs2, work)
        mylist_rforeach(xs.xs1, work)


