



#use ".assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;


  





  (*let string_avoid_132 cs =
    let rec av (start:string) acc= 
  
    if string_length start < 3  then true else
      match acc with
      | false -> false
      | _ -> if ord (string_get_at start 0) < ord (string_get_at start 2) &&  ord (string_get_at start 2)<  ord (string_get_at start 1) then false else 
        true in
        
    av (string_tail cs) true
*)
  
      


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

let rec string_avoid_132 cs =
  let n = string_length cs in

  let rec find_132 i =
    if i >= n - 2 then
      true  (* Reached the end without finding a 132-like subsequence *)
    else
      let a = string_get_at cs (i) in
      let b = string_get_at cs (i+1) in
      let c = string_get_at cs (i+2) in
      if a < c && c < b then
        false  (* Found a 132-like subsequence *)
      else
        find_132 (i + 1)
  in

  find_132 0



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






