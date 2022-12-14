open Base

type t = Empty
exception Not_implemented
exception Invalid_case

type cave_map_elt = Air | Rock | Sand
type cave_map = cave_map_elt array
type coord = (int * int)

type input = Input of cave_map
type answer = Answer of int | Unknown


(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* Parse input functions *)
(* Input example to parse
    498,4 -> 498,6 -> 496,6
    503,4 -> 502,4 -> 502,9 -> 494,9
*)
let max_x = 1000
let max_y = 200

(* x grows right, y growns down *)
let coord_to_idx ((x,y) : int * int) : int = x + y * max_x
let idx_to_coord (idx : int) : (int * int) = (idx % max_x, idx / max_x)
let check_points (line:string) : (int list) =
  let open List in
  line 
  |> String.split ~on:' '
  |> filter ~f:(fun x -> String.(<>) x "->")
  |> map ~f:(fun x -> String.split x ~on:',')
  |> map ~f:(fun x -> (Int.of_string @@ nth_exn x 0, Int.of_string @@ nth_exn x 1))
  |> map ~f:coord_to_idx

let build_path (idx0: int) (idx1: int) : (int list) =
  let (x0, y0) = idx_to_coord idx0 in 
  let (x1, y1) = idx_to_coord idx1 in
  let (dx, dy) = ((x1-x0) , (y1-y0)) in
  let sign (x:int) : int = if x < 0 then -1 else if x > 0 then +1 else 0 in
  let coord_path = 
    match (dx, dy) with
    | (0,0) -> []
    | (0, dy) -> List.init (abs dy) ~f:(fun n -> (x0, (n+1) * (sign dy) + y0))
    | (dx, 0) -> List.init (abs dx) ~f:(fun n -> ((n+1) * (sign dx) + x0, y0))
    | _ -> raise Invalid_case
  in
  List.map coord_path ~f:coord_to_idx

let one_line_tajectory (line:string) : (int list) =
  let check_points = check_points line in
  List.fold check_points ~init:[] ~f:(
    fun path next_idx ->
      match path with 
      | [] -> [next_idx]
      | prev_idx::_ -> List.rev_append (build_path prev_idx next_idx) path)

let text_to_input (t: string) : input =
  let rocks = t |> String.split_lines |> List.concat_map ~f:one_line_tajectory in
  let a = Array.init (max_x * max_y) ~f:(fun _ -> Air) in
  List.iter rocks ~f:(fun idx -> Array.set a idx Rock); 
  Input a

(* Debug functions *)


(* Solution helper functions *)
let find_place_for_sand_unit (cave: cave_map) (start_point: int) : (int option) =
  let lookup (idx: int) : cave_map_elt = Array.get cave idx in
  let rec iter (idx: int) : (int option) =
    let coord = idx_to_coord idx in
    let (x, y) = coord in
    let down   = coord_to_idx (x + 0, y + 1) in
    let down_l = coord_to_idx (x - 1, y + 1) in
    let down_r = coord_to_idx (x + 1, y + 1) in
    if y > max_y then None 
    else match (lookup down, lookup down_l, lookup down_r) with
    | (_ , Air , _) -> iter down
    | (Air , _ , _) -> iter down_l
    | (_ , _ , Air) -> iter down_r
    | _ -> Some @@ coord_to_idx coord
  in 
  iter start_point

let fill_cave_with_sand 

(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
