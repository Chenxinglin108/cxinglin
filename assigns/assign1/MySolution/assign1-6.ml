



#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;




let rec string_avoid_1324 cs =
  let n = string_length cs in

  let rec find_1324 i =
    if i >= n - 3 then
      true  (* Reached the end without finding a 1324-like subsequence *)
    else
      let a = string_get_at cs (i) in
      let b = string_get_at cs (i+1) in
      let c = string_get_at cs (i+2) in
      let d = string_get_at cs (i+3) in   
      if a < c && c < b && b<d then
        false  (* Found a 1324-like subsequence *)
      else
        find_1324 (i + 1)
  in

  find_1324 0

