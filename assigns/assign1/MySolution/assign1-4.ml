(* ****** ****** *)
(*
Assign1: Onward!

Total: 70 points + 20 bonus points
 
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
*)
(* ****** ****** *)

(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.






For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.

fun intrev10(n: int): int

*)




 



(* ****** ****** *)

(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)

(* ****** ****** *)

(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
check if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

(* ****** ****** *)

(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun intrep(ds1: int)(ds2: int): int					     

For instance, "6123" + "987" = "7110"







Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)



let chr = Char.chr
let ord = Char.code


let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in ord(ch) - ord('0')

  
  
    let char_of_digit (d0: int): char =
      let () = assert(d0 >= 0) in
        let () = assert(d0 <= 9) in 
          chr(ord('0') + d0)

  let string_get_at(cs:string)(i0:int): char = String.get cs i0

  let string_init = String.init
  let string_length = String.length 
  let string_cons(c0: char)(cs: string): string = 
  string_init(string_length(cs) + 1)(
    fun i -> if i <= 0 then c0 else string_get_at cs (i-1))
  
    let str(c0) = String.make 1 c0

    let string_tail(cs) =
string_init(string_length(cs)-1)(fun i -> string_get_at(cs)(i+1))

let string_snoc(cs: string)(c0: char): string =
  let len = string_length(cs) in
    string_init(len + 1)(
      fun i -> if i < len then string_get_at (cs) (i) else c0
    )


    let reverse s =
       let rec helper i =
          if i >= string_length s then "" else  string_snoc  (helper (i+1))  (string_get_at s i)

        in  helper 0





let intrep ds1 ds2 = 
  let result=
  let rec helper s1 s2  c =

  match (s1,s2) with

  | "",""-> if c = 1 then string_cons (char_of_digit c) ""  else ""
  |"" , _-> if c + digit_of_char(string_get_at s2 0) >9 then  string_cons '0'  (helper  ""  (string_tail s2)  1)    else   (string_cons  (char_of_digit (  c+ digit_of_char(string_get_at s2 0) ) ) (helper "" (string_tail s2) 0))
  |_, "" -> if c + digit_of_char(string_get_at s1 0) > 9 then  string_cons '0'  (helper  ""  (string_tail s1)  1)    else   (string_cons  (char_of_digit (  c+ digit_of_char(string_get_at s1 0) ) ) (helper "" (string_tail s1) 0))
  |_,_-> if  (c+digit_of_char(string_get_at s1 0) + digit_of_char(string_get_at s2 0) )> 9 then string_cons (char_of_digit (digit_of_char(string_get_at s1 0) + c+ digit_of_char(string_get_at s2 0 )-10)) (helper (string_tail s1) (string_tail s2) 1)  
  
  else string_cons(  char_of_digit (c+digit_of_char(string_get_at s1 0) + digit_of_char(string_get_at s2 0 ) ))   (helper (string_tail s1) (string_tail s2) 0)       

  

in helper (reverse ds1) (reverse ds2) 0

in reverse result




(* ****** ****** *)

(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)

(* ****** ****** *)

(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
check if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)

(* ****** ****** *)

(* end of [CS320-2023-Fall-assigns-assign1.ml] *)






let intrev10 n =
    let rec helper n reversed =
      if n = 0 then
        reversed
      else
        let last_digit = n mod 10 in
        let new_reversed = reversed * 10 + last_digit in
        let remaining = n / 10 in
        helper remaining new_reversed
    in
    helper n 0




    

   