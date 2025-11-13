(* Graphics module for 2048 game with colored tiles *)

open Graphics

(* 2048 tile colors - classic color scheme *)
let color_of_value = function
  | 0 -> rgb 205 193 180    (* Empty tile - #cdc1b4 *)
  | 2 -> rgb 238 228 218    (* #eee4da *)
  | 4 -> rgb 237 224 200    (* #ede0c8 *)
  | 8 -> rgb 242 177 121    (* #f2b179 *)
  | 16 -> rgb 245 149 99    (* #f59563 *)
  | 32 -> rgb 246 124 95    (* #f67c5f *)
  | 64 -> rgb 246 94 59     (* #f65e3b *)
  | 128 -> rgb 237 207 114  (* #edcf72 *)
  | 256 -> rgb 237 204 97   (* #edcc61 *)
  | 512 -> rgb 237 200 80   (* #edc850 *)
  | 1024 -> rgb 237 197 63  (* #edc53f *)
  | 2048 -> rgb 237 194 46  (* #edc22e *)
  | _ -> rgb 60 58 50       (* Dark for higher values *)

(* Text color based on tile value - dark text for light tiles, light text for dark tiles *)
let text_color_of_value = function
  | 2 | 4 -> rgb 119 110 101  (* Dark text for light tiles *)
  | _ -> rgb 249 246 242      (* Light text for dark tiles *)

(* Window dimensions *)
let window_width = 500
let window_height = 600
let board_size = 4
let tile_size = 100
let padding = 10
let board_offset_x = 50
let board_offset_y = 100
let tile_padding_x = 5
let tile_padding_y = 12

(* Initialize graphics window *)
let init_graphics () =
  open_graph (Printf.sprintf " %dx%d" window_width window_height);
  set_window_title "2048";
  auto_synchronize false

(* Draw a single tile *)
let draw_tile x y value =
  let screen_x = board_offset_x + x * (tile_size + padding) in
  let screen_y = board_offset_y + (board_size - 1 - y) * (tile_size + padding) in
  
  (* Draw tile background *)
  set_color (color_of_value value);
  fill_rect screen_x screen_y tile_size tile_size;
  
  (* Draw tile border *)
  set_color (rgb 187 173 160);
  draw_rect screen_x screen_y tile_size tile_size;
  
  (* Draw number if not empty *)
  if value <> 0 then (
    set_color (text_color_of_value value);
    set_text_size 45;
    let text = string_of_int value in
    (* Estimate text width: approximately 20 pixels per character for size 36 *)
    let text_width = String.length text * 20 in
    let text_x = screen_x + (tile_size - text_width + tile_padding_x) / 2 in
    let text_y = screen_y - tile_padding_y + tile_size / 2 + 10 in
    moveto text_x text_y;
    draw_string text
  )

let draw_tile_text x y s text_color =
  (* outline first *)
  set_color (rgb 80 80 80);
  List.iter (fun (dx,dy) ->
    moveto (x+dx) (y+dy); draw_string s
  ) [(-1,0); (1,0); (0,-1); (0,1)];

  (* then actual "bold" text *)
  set_color text_color;
  moveto x y;
  draw_string s

(* Draw the entire board *)
let draw_board (board : int array array) =
  clear_graph ();
  
  (* Draw background *)
  set_color (rgb 250 248 239);
  fill_rect 0 0 window_width window_height;
  
  (* Draw title *)
  draw_tile_text 250 (window_height - 60) "2048" (rgb 0 0 0);
  
  (* Draw instructions *)
  set_text_size 14;
  moveto 50 50;
  set_color (rgb 0 0 0);
  draw_string "Use WASD to move. Press R to restart. Press Q to quit.";
  
  (* Draw all tiles *)
  for i = 0 to board_size - 1 do
    for j = 0 to board_size - 1 do
      draw_tile j i board.(i).(j)
    done
  done;
  
  synchronize ()

(* Convert Graphics key to direction *)
let direction_of_key = function
  | 'w' | 'W' -> Some Cos326_2048.UP
  | 's' | 'S' -> Some Cos326_2048.DOWN
  | 'a' | 'A' -> Some Cos326_2048.LEFT
  | 'd' | 'D' -> Some Cos326_2048.RIGHT
  | _ -> None

(* Game loop with graphics *)
let rec game_loop (b : int array array) =
  draw_board b;
  
  (* Check if game is over *)
  let game_over = Cos326_2048.is_game_over b in
  if game_over then (
    set_color (rgb 0 0 0);
    set_text_size 24;
    moveto 100 (window_height / 2 - 20);
    draw_string "Game Over! No more moves.";
    synchronize ();
    (* Wait for key press *)
    let status = wait_next_event [Key_pressed] in
    match status.key with
    | 'r' | 'R' -> 
        let new_board = Cos326_2048.create_new_board () in
        game_loop new_board
    | 'q' | 'Q' -> 
        close_graph ();
        print_endline "Thanks for playing!"
    | _ -> game_loop b
  ) else (
    (* Check for win condition *)
    let has_2048 = 
      Array.exists (fun row -> 
        Array.exists (fun x -> x = 2048) row) b in
    if has_2048 then (
      set_color (rgb 0 0 0);
      set_text_size 24;
      moveto 150 (window_height / 2);
      draw_string "You Win! Press Q to quit.";
      synchronize ()
    );
    
    (* Wait for key press *)
    let status = wait_next_event [Key_pressed] in
    match status.key with
    | 'q' | 'Q' -> 
        close_graph ();
        print_endline "Thanks for playing!"
    | 'r' | 'R' -> 
        let new_board = Cos326_2048.create_new_board () in
        game_loop new_board
    | key ->
        match direction_of_key key with
        | Some dir ->
            let (new_board, won) = Cos326_2048.move_board b dir in
            (* Check if board changed by comparing each cell *)
            let board_changed = ref false in
            for i = 0 to 3 do
              for j = 0 to 3 do
                if b.(i).(j) <> new_board.(i).(j) then
                  board_changed := true
              done
            done;
            if !board_changed then (
              if won then (
                (* Game won by merging 516 tiles *)
                draw_board new_board;
                set_color (rgb 119 110 101);
                set_text_size 24;
                moveto 100 (window_height / 2);
                draw_string "You won!";
                synchronize ()
              );
              let new_board_val = Cos326_2048.generate_new_number new_board in
              game_loop new_board_val
            ) else (
              game_loop new_board
            )
        | None ->
            game_loop b
  )

(* Start the graphics game *)
let start_graphics_game () =
  Random.self_init ();
  init_graphics ();
  let initial_board = Cos326_2048.create_new_board () in
  game_loop initial_board

(* Main entry point for executable *)
let () = start_graphics_game ()

