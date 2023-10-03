


#use ".assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;



type board_t = int * int * int * int * int * int * int * int

let is_safe board row col =
  let check_diag x y =
    let dx = abs (row - x) in
    let dy = abs (col - y) in
    dx <> dy
  in
  let rec check_rows r =
    match r with
    | -1 -> true
    | r ->
      let c = List.nth board r in
      c <> col && check_diag r c && check_rows (r - 1)
  in
  check_rows (row - 1)

let init_board size =
  let rec aux n acc =
    if n = 0 then acc else aux (n - 1) (0 :: acc)
  in
  aux size []

let solve_queen_puzzle size =
  let init = init_board size in
  let rec place_queen board row =
    if row = size then [board]
    else
      List.fold_left
        (fun acc col ->
          if is_safe board row col then
            let new_board = List.mapi (fun i c -> if i = row then col else c) board in
            acc @ place_queen new_board (row + 1)
          else
            acc
        )
        []
        (List.init size (fun x -> x))
  in
  place_queen init 0

let queen8_puzzle_solve () =
  let size = 8 in
  let solutions = solve_queen_puzzle size in
  List.map (fun board -> Array.of_list board) solutions

let () =
  let solutions = queen8_puzzle_solve () in
  Printf.printf "Number of solutions: %d\n" (List.length solutions);
  List.iter (fun board -> Array.iter (fun col -> Printf.printf "%d " col) board; print_newline ()) solutions
