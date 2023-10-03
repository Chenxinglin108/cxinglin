


#use ".assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;



let is_safe board row col =
  let check_diag x y =
    let dx = abs (row - x) in
    let dy = abs (col - y) in
    dx <> dy
  in
  let rec check_rows r =
    if r < row then
      let c = board.(r) in
      c <> col && check_diag r c && check_rows (r + 1)
    else
      true
  in
  check_rows 0

let solve_queen_puzzle size =
  let solutions = ref [] in
  let board = Array.make size 0 in

  let rec place_queen row =
    if row = size then
      solutions := Array.copy board :: !solutions
    else
      for col = 0 to size - 1 do
        if is_safe board row col then begin
          board.(row) <- col;
          place_queen (row + 1);
        end
      done
  in

  place_queen 0;
  !solutions

let print_solution board =
  Array.iter (fun col ->
    for i = 0 to Array.length board - 1 do
      if i = col then print_string "Q " else print_string ". ";
    done;
    print_newline ()
  ) board;
  print_newline ()