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


class MyList:
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def __str__(self):
        return str(self.head) + ", " + str(self.tail)

    def __repr__(self):
        return "MyList(" + repr(self.head) + ", " + repr(self.tail) + ")"

    def mylist_foldl(self, fn, initial):
        if self.tail is None:
            return fn(initial, self.head)
        else:
            return self.tail.mylist_foldl(fn, fn(initial, self.head))

    def mylist_foldr(self, fn, initial):
        if self.tail is None:
            return fn(self.head, initial)
        else:
            return fn(self.head, self.tail.mylist_foldr(fn, initial))

    def mylist_map(self, fn):
        if self.tail is None:
            return MyList(fn(self.head), None)
        else:
            return MyList(fn(self.head), self.tail.mylist_map(fn))
        
        
        
        
        
def mylist_foreach(lst, fn):
    if lst is not None:
        fn(lst.head)
        mylist_foreach(lst.tail, fn)

def mylist_rforeach(lst, fn):
    if lst is not None:
        mylist_rforeach(lst.tail, fn)
        fn(lst.head)
