#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 25 17:48:47 2023

@author: jczs
"""

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

def string_merge(cs1, cs2):
    n1 = len(cs1)
    n2 = len(cs2)

    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                c1 = cs1[i1]
                c2 = cs2[i2]
                if c1 <= c2:
                    work(c1)
                    foreach(i1 + 1, i2, work)
                else:
                    work(c2)
                    foreach(i1, i2 + 1, work)
            else:
                for i in range(i1, n1):
                    work(cs1[i])
        else:
            for i in range(i2, n2):
                work(cs2[i])

    result = string_make_fwork(lambda work: foreach(0, 0, work))
    return result

def string_make_fwork(f):
    result = []
    
    def work(x):
        result.append(x)

    f(work)
    return ''.join(result)

def int1_foreach(n, f):
    for i in range(n):
        f(i)
