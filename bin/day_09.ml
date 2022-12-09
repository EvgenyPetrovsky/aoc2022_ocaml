open Base

exception Not_implemented
exception Unexpected_case of string

type move = U | D | L | R
type pos = (int * int) 
type rope = { knots : pos list }

type input = Input of move list
type answer = Answer of int | Unknown

(* get one line and return the list of moves *)
let parse_one_line (line: string) : (move list) =
  let spl = String.split line ~on:' ' in
  let dir = match List.hd_exn spl with 
  | "U" -> U 
  | "D" -> D 
  | "L" -> L 
  | "R" -> R
  | x -> raise (Unexpected_case x)
  in
  let num = Int.of_string @@ List.nth_exn spl 1 in
  List.init num ~f:(fun _ -> dir)

let text_to_input (t: string) :input = 
  t
  |> String.split_lines
  |> List.map ~f:parse_one_line
  |> List.concat
  |> (fun x -> Input x)

(* change position of head *)
let move_head ((x, y) : pos) (direction: move) : pos =
  match direction with
  | U -> (x, y+1)
  | R -> (x+1, y)
  | D -> (x, y-1)
  | L -> (x-1, y)
(* tail can't be more than 1 position away *)
let move_tail ~(tail : pos) ~(head : pos) : pos =
  let ((xh, yh), (xt, yt)) = (head , tail) in
  let (dx, dy) = (xh-xt, yh-yt) in
  let new_x = match (abs dy) with
  | 0 | 1 -> (match dx with | 0 -> xt | _ -> xh - (dx / abs(dx)))
  | _ -> xh
  in
  let new_y = match (abs dx) with 
  | 0 | 1 -> (match dy with | 0 -> yt | _ -> yh - (dy / abs(dy)))
  | _ -> yh
  in
  (new_x , new_y)

let move_rope ({knots}: rope) (move: move) : rope =
  let (head, tail) = (List.hd_exn knots, List.tl_exn knots) in
  let new_head = move_head head move in 
  let rec move_knots (knot: pos) (next_knots: pos list) : (pos list) = 
    match next_knots with
    | [] -> []
    | nk::nks ->
      let new_pos = move_tail ~tail:nk ~head:knot in 
      new_pos :: (move_knots new_pos nks)
  in
  (*Stdio.printf "{ head = (%d,%d) ; tail = %d,%d }\n" (fst new_head) (snd new_head) (fst new_tail) (snd new_tail);*)
  {knots = new_head :: (move_knots new_head tail)}


(* Solution for part 1 *)
let part1 (Input moves : input) : answer = 
  let num_knots = 2 in
  let start = [{knots = List.init num_knots ~f:(fun _ -> (0,0))}] in
  let record_move acc move = (move_rope (List.hd_exn acc) move) :: acc in
  let pos_compare ((x1,y1) : int * int) ((x2,y2) : int * int) : int =
    match (x2 - x1, y2 - y1) with
    | (0, dy) -> dy
    | (dx, _) -> dx
  in
  moves 
  |> List.fold ~init:start ~f:record_move
  |> List.map ~f:(fun {knots} -> List.last_exn knots)
  |> List.dedup_and_sort ~compare:pos_compare
  |> List.length
  |> (fun x -> Answer x)

(* Solution for part 1 *)
let part2 (Input moves : input) : answer =
let num_knots = 10 in
let start = [{knots = List.init num_knots ~f:(fun _ -> (0,0))}] in
let record_move acc move = (move_rope (List.hd_exn acc) move) :: acc in
let pos_compare ((x1,y1) : int * int) ((x2,y2) : int * int) : int =
  match (x2 - x1, y2 - y1) with
  | (0, dy) -> dy
  | (dx, _) -> dx
in
moves 
|> List.fold ~init:start ~f:record_move
|> List.map ~f:(fun {knots} -> List.last_exn knots)
|> List.dedup_and_sort ~compare:pos_compare
|> List.length
|> (fun x -> Answer x)

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"