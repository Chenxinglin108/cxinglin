




#use ".assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let theNatPairs: (int*int) strcon = 

  let rec pairs i j  () =
   match (i,j) with 
   | (0,_) ->
      StrCons ((i, j), fun () ->
      pairs (i + 1) (j - 1) ())

    | (a,0)->
      StrCons ((i, j), fun () ->
        pairs (0) (a+1) ()
      )

  | (_,_) ->
        StrCons ((i, j), fun () ->
        pairs (i + 1) (j - 1) ())
  
  in
  StrCons ((0, 0), fun () -> pairs 0 1  () )