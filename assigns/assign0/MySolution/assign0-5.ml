#use "./../assign1.ml";;
 #use "./../../../classlib/OCaml/MyOCaml.ml";;



 let chr = Char.chr
 let ord = Char.code
 let str(c0) = String.make 1 c0
 
 (* ****** ****** *)
 
 let string_init = String.init
 let string_length = String.length
 let string_get(cs, i0) = String.get cs i0

 let stringrev(cs: string): string =
  let len = string_length cs in
  String.init len (fun i -> string_get (cs ,(len - 1 - i)))





