################################################
#
# Midterm Review
#
################################################

## Names vs. Variables

Variables are mutable,
but Names are immutable;
like 'const' in C++; like 'final' in Java

################################################

Names imitating variables (thru 'shadowing'):

let x = 1 in
let x = x + 1 in ...

let (x, y) = (1, 2)
let swap(x)(y) = (y, x)
let (x, y) = swap(x)(y) (* (x, y) = (2, 1) *)
    
################################################

## Recursive functions

Functions are first-class values

## Tail-Recursion

Loops are tail-recursive functions (However, many popular mainstream
programming languages (e.g., Python) does not support tail-recursive
call optimization, which is rather unfortuate!)

let rec
f91(x: int): int =
if x > 100 then x - 10 else f91(f91(x+11))

Two recursive calls:
The outer one is tail-recursive;
the inner one is not tail-recursive

Hence, f91 is not a tail-recursive function
(
because not all the involved recursive calls
are tail-calls.)

let foo(x) = x + 5 (*foo is non-recursive*)
(*
foo is both non-recursive and tail-recursive
*)

(*
A challenging question:
How do you implement f91 in a tail-recursive
manner?
*)

################################################

## Higher-order functions

Loops are 2nd-order functions
Loop transformers are 3rd-order functions

foreach_to_forall: transformer
foreach_to_get_at: transformer
foreach_to_map_list: transformer
foldleft_to_iforeach: transformer
z2foreach, x2foreach, z2map_list, x2map_list, etc.

################################################

## Mutual Recursion

let rec
isevn(x: int): bool =
if x = 0 then true else isodd(x-1)
and
isodd(x: int): bool =
if x = 0 then false else isevn(x-1)

The involved recursive calls are tail-calls.
Note that isevn and isodd are defined mutually
tail-recursively. Hence, isevn/isadd are tail
recursive functions.

################################################

How to rewrite a non-tail-recursive function

For instance,

let rec
fact(x) =
if x > 0 then x * fact(x-1) else 1

The above code can be rewritten into the follow
one with the use of an 'accumulator':

let
fact(x) =
let rec loop(i)(r) =
if i < x
then loop(i+1)((i+1)*r) else r in loop(0)(1)
end

The above implementations corresponds to the
following combinator-based one:

let
fact(x) = int1_foldleft(x)(1)(fun r i -> (i+1)*r)

################################################

## Datatypes and Pattern Matching

There features are in the core of ML-like
functional programming languages; they, for
instance, can be used to greatly facilitate
compiler construction. You will see more of these
features in Part II of this class.

Here are some common datatypes:

(* non-recursive *)
type 'a option =
  None | Some of 'a

(* list is recursive *)
type 'a list =
  Nil | Cons of 'a * 'a list

let
option_length(xs) =
  None -> 0
| Some _ -> 1

let rec
list_length(xs) =
  Nil -> 0
| Cons(_, xs) ->
  1 + list_length(xs)

let
list_length(xs) =
  Nil -> 0
| Cons(_, xs) ->
  1 + list_length(xs) (* no recursive call!!! *)

## Option-based error handling
## Exception-based error handling

Please read Chapter 29 of the book by Robert Harper.

################################################

let
list_head(xs: 'a list): 'a =
match xs with
  [] -> raise Empty | x1 :: _ -> x1

################################################

let
foreach_to_head
(foreach: ('xs, 'x0) foreach): xs -> 'x0 =
fun xs ->
let
exception Found of 'x0 in
try
foreach(xs)
(fun x -> raise Found(x)); raise Empty
with Found(x) -> x

################################################

(* This one appeared on Quiz2 *)

let
list_last(xs: 'a list): 'a =
match
list_foldright
(xs)(None)
(fun x r ->
match r with
None -> Some(x) | _ -> r)
with
| Some x -> x | None -> raise Empty

################################################

## Continuation

(* direct-style *)
let rec
fact(x) =
if x > 0 then x * fact(x-1) else 1

(* CPS-style of fact *)
let
rec
kfact
(x: int)
(k: int -> 'ans): 'ans =
if x > 0 then
kfact(x-1)(fun res -> k(x * res)) else k(1)

################################################

A little twist: What does kfact2 do?

let
rec
kfact2
(x: int)
(k: int -> int): int =
if x > 0 then
kfact2(x-1)(fun res -> k(x * res)) else k(k(1))

################################################

kfact2(3)(fun res -> res) = ?

kfact2(3)(fun res -> res) ->
kfact2(2)(fun res -> 3 * res) ->
kfact2(1)(fun res -> 2 * (3 * res)) ->
kfact2(0)(fun res -> 1 * (2 * (3 * res))) -> 36

################################################

CPS-translation of tail-recursive functions
is easy/straightforward:

let
kfact(x)(k) =
let rec kloop(i)(r)(k) =
if i < x
then kloop(i+1)((i+1)*r)(k) else k(r) in loop(0)(1)(k)
end

################################################

CPS-translation of tail-recursive functions
is easy/straightforward:

let rec
kisevn(x: int)(k): bool =
if x = 0 then k(true) else kisodd(x-1)(k)
and
kisodd(x: int)(k): bool =
if x = 0 then k(false) else isevn(x-1)(k)

## Generators (LINEAR streams)

(*
Try to understand what 'yield from' does
in the following code
*)

def g_append(gxs, gys):
  yield from gxs
  yield from gys

Please read the following tutorial on this:

https://py.mit.edu/fall23/readings/generators

################################################

Good luck!

################################################


There is a chapter (Chapter 29) in the above book on continuation.

#####################################

Continuation-1:

let rec f91(x) = if x > 100 then x - 10 else f91(f91(x+11))

Please translate f91 into kf91, which is the CPS version of f91.

#####################################

Continuation-2:

let rec kfact(x)(k) =

if x > 0 then kfact(x-1)(fun res -> k(x * res)) else k(1)

kfact is the CPS version of the usual factorial implementation.

Say we have

let rec kfact2(x)(k) =

if x > 0 then kfact(x-1)(fun res -> k(x * res)) else k(k(1))

What is the value if kfact2(3)(fun res -> res)?

You should not run the code on a computer. Instead, use reasoning.

#####################################

Continuation-related:

How do you implement f91 in a tail-recursive manner

(with no use of continuation)?

#####################################

let whatisit1(f)(x) = f(x)(x)

let whatisit2(f)(x)(y) = f(y)(x)

What is the type of whatisit1 in OCaml?

What is the most general type of 'whatisit2' in OCaml?

######################################

let rec
f(x) = f(g(x+1))
and
g(x) = g(g(x-2)) + 1

Please CPS translate f and g simultaneously. The answer is given as follows:

let rec
kf(x)(k) =
kg(x+1)(fun r -> kf r k)
and
kg(x)(k) =
kg(x-2)(fun r -> kg r (fun r -> k(r + 1)))

####################################

let rec f(x) = f(2*x)

What would happen if you evaluate 'f(1)' in OCaml?

def f(x):

     return f(2*x)

What would happen if you evaluate 'f(1)' in Python?

######################################

let rec f(x) = if x <> 0 then f(2*x) else x

If you evaluate f(1) in OCaml, what will happen? Why?

######################################

Question 1
let fg = fun(f)(g)(x) -> f(g(x))


What is the type of fg in OCaml?
1. (’a -> ’a) -> (’b -> ’b) -> (’a -> ’b)
2. (’a -> ’b) -> (’b -> ’c) -> (’a -> ’c)
3. (’a -> ’b) -> (’a -> ’b) -> (’a -> ’b)
4. (’b -> ’c) -> (’a -> ’b) -> (’a -> ’c)
5. None of the above

###############################


let
foo = fun x -> x

let

foo = fun x ->
if x > 0 then x * foo(x-1) else 1

let foo1 = foo(1)
let foo2 = foo(2)

What are the values of foo1 and foo2

#####################################

let rec
foo = fun x -> x

let rec

foo = fun x ->
if x > 0 then x * foo(x-1) else 1

let foo1 = foo(1)
let foo2 = foo(2)

What are the values of foo1 and foo2

###################################

(*
 This question should reinforce the claim that
 references are to be avoided.
*)

let
tricky =
let
global = ref(0) in
let rec
f(i: int) : int =
global := !global + i;                                                                                                                                
if i > 0
then f(i-1) else !global in fun(i:int) -> f(i)
;;                                                                                                                                                    
(*
 What is the value of the expression tricky(10)?
*)

let tricky10 = tricky(10);;         

####################################

let
mystream =
let
rec
fstream(n: int): int stream = fun() ->
StrCons(n, stream_map(fstream(n+1))(fun(x) -> x+x+1)) in
fstream(0)
;;                                                                                                                                                    
(*
 The first element of mystream is 0.
 What is the 5th (fifth) element in mystream?
 *)
;;

##################################

let rec
fff(n: int): int =
if n = 0 then 0 else 10*fff(n / 2) + n mod 2
;;                                                                                                                                                    
(*
 What is the value of fff(1023)?
*)

##################################

type intcont = (int) -> int;;                                                                                                                         

let rec
kf(n: int)(k1: intcont)(k2: intcont): int =
if n = 0
then k1(0) else
kf(n-1)(fun(res) -> k2(res+n))(fun(res) -> k1(res-n))
;;                                                                                                                                                    
let
kf0(n: int): int = kf(n)(fun res -> res)(fun res -> res)
;;                                                                                                                                                    

(*
 What is the value of kf0(10)?
*)