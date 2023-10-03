

#use ".assign3.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;
let list_foldleft(xs) =
  foreach_to_foldleft(list_foreach)(xs)

let list_foldright(xs) =
  rforeach_to_foldright(list_rforeach)(xs)
let
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
list_foldright(xs)([])(fun x0 r0 -> fopr(x0) :: r0)




    let substring str start_pos end_pos =
      let str_length = string_length str in
     
      if start_pos < 0 || start_pos >= str_length || end_pos <= start_pos || end_pos > str_length then
        ""
      else
        let length = end_pos - start_pos  in
        string_init length (fun i -> string_get_at str (start_pos + i))

      
        let substring2 str start_pos end_pos =
          let str_length = string_length str in

          if start_pos = end_pos then string_cons (string_get_at str start_pos) "" else
         
          if start_pos < 0 || start_pos >= str_length || end_pos <= start_pos || end_pos > str_length then
            ""
          else
            let length = end_pos - start_pos +1  in
            string_init length (fun i -> string_get_at str (start_pos + i))


            let list_init=List.init

    let helper word =
      let n = string_length word in
      let buddies = list_init n (fun i ->
        let prefix = substring word 0 i in
        let suffix = string_append ("_") (substring2 word (i + 1) (n-1 )) in
        (string_append prefix  suffix) 
      ) in buddies

      let replace_char str old_char new_char =
        let result = string_init (string_length str) (fun i ->
          if string_get_at str i = old_char then new_char
          else string_get_at str i
        ) in
        result

let list_length xs= list_foldleft xs 0 (fun x _ -> x + 1 )

        let r str char_list =
          let char_list_len = list_length char_list in
          if char_list_len = 0 then
            [str]  (* Return a list with the original string if the char list is empty *)
          else
            let replace_char new_char =
              string_init (string_length str) (fun i ->
                if string_get_at str i = '_'  then new_char else string_get_at str i
              )
            in
            list_map  char_list replace_char

            let x str_list char_list r =
              list_map str_list  (fun str ->
               r str char_list
              ) 


            let
            list_of_buddies(word: string)=
            
            let string1 = list_foldleft  (x (helper(word)) (list_init 26 (fun i -> chr (int_of_char 'a' + i))) (r)) [] list_append in 

            list_foldleft string1 [] (fun acc x -> if x= word then acc else x :: acc) 





           
    (*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*








