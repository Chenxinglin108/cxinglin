#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 25 17:35:43 2023

@author: jczs
"""

########################################################################
# Assign2:
# //
# DUE: the 27th of September, 2023
# //
# Total: 80 point
# (OCaml: 40 points)(Python: 40 points)
########################################################################
# Higher-order programming in Python
########################################################################
#
# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml
#
########################################################################
#
# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly
#
# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in
# let rec
# foreach(i1: int)(i2:int)(work) =
# if
# i1 < n1
# then (
# if
# i2 < n2
# then
#   let c1 = string_get_at(cs1)(i1)
#   and c2 = string_get_at(cs2)(i2) in
#   if c1 <= c2
#   then (work(c1); foreach(i1+1)(i2+0)(work))
#   else (work(c2); foreach(i1+0)(i2+1)(work))
# else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
# ) else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;
#
########################################################################

############## end of [CS320-2023-Fall-assigns-assign2.py] ##############


####################################################
#!/usr/bin/env python3
####################################################
import sys
####################################################
sys.setrecursionlimit(16000)
####################################################
sys.path.append('./../../MySolution/Python')
sys.path.append("./../../../../classlib/Python")
####################################################
from MyPython import *
from assign2_5 import *


######################################################




class fnlist:
    ctag = -1

    def get_ctag(self):
        return self.ctag

    def __iter__(self):
        return fnlist_iter(self)

    def __reversed__(self):
        return fnlist_reverse(self)


class fnlist_iter:
    def __iter__(self):
        return self

    def __init__(self, itms):
        self.itms = itms

    def __next__(self):
        if self.itms.ctag == 0:
            raise StopIteration
        else:
            itm1 = self.itms.cons1
            self.itms = self.itms.cons2
            return itm1


class fnlist_nil(fnlist):
    def __init__(self):
        self.ctag = 0


class fnlist_cons(fnlist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons1
        self.cons2 = cons2

    def get_cons1(self):
        return self.cons1

    def get_cons2(self):
        return self.cons2


def fnlist_sing(x0):
    res = fnlist_nil()
    res = fnlist_cons(x0, res)
    return res


def fnlist_make_fwork(fwork):
    res = fnlist_nil()

    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)

    fwork(work)

    # Reverse the fnlist before returning
    reversed_res = fnlist_nil()
    for x in res:
        reversed_res = fnlist_cons(x, reversed_res)

    return reversed_res



