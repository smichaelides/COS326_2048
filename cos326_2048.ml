(* 
2048 game invariants:
- When shift happens, alike numbers merged and doubled & fill padding with 0
- One new number 2 or 4 is added to board each time shift happens 
- Game ends when one of the cells reaches 2048 
- Game ends when player presses ESC (no more moves possible) 
*)

type direction = UP | DOWN | LEFT | RIGHT

(* 4x4 board of ints; 0 means empty *)
type board = int array array

(* helper functions for array manipulation *)
let length arr = Array.length arr
let init n f = Array.init n f
let rev (arr : 'a array) : 'a array =
  let n = Array.length arr in
  Array.init n (fun i -> arr.(n - 1 - i))
let to_list arr = Array.to_list arr
let of_list lst = Array.of_list lst
let map f arr = Array.map f arr

(* helper functions for list manipulation *)
let list_length lst = List.length lst
let list_init n f = List.init n f
let list_rev lst = List.rev lst
let filter f lst = List.filter f lst

(* compression helper functions that removes 0s *)
let compress_row (row: int array) : int list =
    to_list row |> filter (fun x -> x <> 0)

(* merge adjacent alike numbers (ONCE) and doubel them *)
(* ie. [2; 2; 2] -> [4; 2] *)
let merge (row: int list) : int list =
    let rec aux row acc = 
    match row with 
    | x :: y :: rest when x = y -> aux rest (2 * x :: acc)
    | x :: rest -> aux rest (x :: acc)
    | [] -> list_rev acc
    in aux row []

(* fill padding with 0s to the end of the list *)
let fill_padding (row: int list) : int list = 
  let len = list_length row in
  let diff = max 0 (4 - len) in
  row @ (list_init diff (fun _ -> 0))

(* reverse a row *)
let reverse_row (row : int array) : int array =
  let n = length row in
  init n (fun i -> row.(n - 1 - i))

(* transpose a board *)
let transpose (b : board) : board =
  let n = length b in
  init n (fun i ->
    init n (fun j -> b.(j).(i)))

(* move left on a single row *)
let move_left_row (row : int array) : int array =
    let nonzeros = compress_row row in
    let merged = merge nonzeros in
    let padded = fill_padding merged in
    of_list padded

(* move left on entire board *)
let move_left (b : board) : board =
    map move_left_row b

(* move right: reverse rows, move left, reverse back *)
let move_right (b : board) : board =
  b |> map reverse_row |> move_left |> map reverse_row

(* move up: transpose, move left, transpose back *)
let move_up (b : board) : board =
  b |> transpose |> move_left |> transpose

(* move down: transpose, reverse rows, move left, reverse back, transpose back *)
let move_down (b : board) : board =
  b |> transpose |> map reverse_row |> move_left |> map reverse_row |> transpose

(* more board in all directions *)
let move_board board direction =
    match direction with
    | UP -> move_up board
    | DOWN -> move_down board
    | LEFT -> move_left board
    | RIGHT -> move_right board

(* convert a char to a direction option *)
let direction_of_char = function
  | 'w' -> Some UP
  | 's' -> Some DOWN
  | 'a' -> Some LEFT
  | 'd' -> Some RIGHT
  | _ -> None

let generate_new_number (b : board) : board =
  let n = length b in
  let empty_cells = 
    List.concat 
      (list_init n (fun i -> 
        list_init n (fun j -> (i, j))))
    |> filter (fun (i, j) -> b.(i).(j) = 0) in
  if empty_cells = [] then b
  else
    let (i, j) = List.nth empty_cells (Random.int (List.length empty_cells)) in
    b.(i).(j) <- if Random.int 100 < 90 then 2 else 4;
    b

(* create a new board with (2 2s or 4s) following 2048 probabilities *)
let create_new_board () : board =
  let n = 4 in
  let all_positions = 
    List.concat 
      (list_init n (fun i -> 
        list_init n (fun j -> (i, j)))) in

  (* randomly select 2 different positions *)
  let shuffled = List.sort (fun _ _ -> Random.int 3 - 1) all_positions in
  let pos1 = List.nth shuffled 0 in
  let pos2 = List.nth shuffled 1 in
  (* pre determiend probabilities of 2 and 4 *)
  let value1 = if Random.int 100 < 90 then 2 else 4 in
  let value2 = if Random.int 100 < 90 then 2 else 4 in
  let (i1, j1) = pos1 in
  let (i2, j2) = pos2 in
  init n (fun i ->
    init n (fun j ->
      if (i, j) = (i1, j1) then value1
      else if (i, j) = (i2, j2) then value2
      else 0))

(* ------------RUNNING THE GAME------------ *)

(* print board ASCII helper *)
let print_board (b : board) =
  Array.iter (fun row ->
    Array.iter (fun x ->
      if x = 0 then Printf.printf ".\t"
      else Printf.printf "%d\t" x
    ) row;
    print_newline ()
  ) b;
  print_newline ()

(* game loop that keeps updating the board *)
let rec game_loop (b : board) =
  print_board b;
  print_endline "Move with WASD (or q to quit):";
  let c = read_line () in
  match c with
  | "q" -> print_endline "Thanks for playing!"; ()
  | _ -> (
      match direction_of_char c.[0] with
      | Some dir ->
          let new_board = move_board b dir in
          if new_board = b then game_loop new_board
          else let new_board_val = generate_new_number new_board in
            game_loop new_board_val
      | None ->
          print_endline "Invalid key — use WASD or q!";
          game_loop b
    )

(* start a new game with a fresh board *)
let start_game () =
  Random.self_init ();
  let initial_board = create_new_board () in
  game_loop initial_board