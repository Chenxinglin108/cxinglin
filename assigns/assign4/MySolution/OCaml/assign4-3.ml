
#use ".assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;





type 'a gtree =
  | GTnil
  | GTcons of 'a * ('a gtree list)




  let rec gtree_streamize_dfs (tree: 'a gtree): 'a stream =
    fun () ->
      match tree with
      | GTnil -> StrNil
      | GTcons (value, children) ->
        let child_streams = List.map gtree_streamize_dfs children in
        let rec concat_streams streams () = 
          match streams with
          | [] -> StrNil
          | stream :: rest -> 
            match stream () with
            | StrNil -> concat_streams rest ()
            | StrCons (value, next) -> StrCons (value, concat_streams (next :: rest))
        in
        StrCons (value, concat_streams child_streams)
  


        let rec concat_stream s1 s2 () =
          match s1 () with
          | StrNil -> s2 ()
          | StrCons (value, next) -> StrCons (value, concat_stream next s2)
        
        let rec bfs (queue: 'a gtree list): 'a stream = 
          fun () ->
            match queue with
            | [] -> StrNil
            | GTnil :: rest -> bfs rest ()
            | GTcons (value, children) :: rest -> 
              let current_stream = fun () -> StrCons (value, bfs (list_append rest children)) in
              concat_stream current_stream (fun () -> StrNil) ()
        
        let gtree_streamize_bfs (tree: 'a gtree): 'a stream =
          bfs [tree]
        