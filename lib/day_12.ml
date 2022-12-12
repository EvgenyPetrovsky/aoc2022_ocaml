open Base


exception Not_implemented

type elevation = Start | End | Elevation of int
type input = Input of { heigth: int ; width: int ; elevations: elevation array }
type path = Path of int list (* list of map_indexes; head element = last step *)
type shortest_path = { map_idx : int ; length_to_it : int }
type answer = Answer of int | Unknown

(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


  (* Parse input functions *)
let text_to_input (t: string) :input =
  let lines = String.split_lines t in
  let heigth = List.length lines in
  let width = String.length @@ List.hd_exn lines in
  let elevation (c: char) : elevation = 
    match c with 
    | 'S' -> Start 
    | 'E' -> End 
    | x -> Elevation (Char.to_int x - Char.to_int 'a')
  in
  lines
  |> String.concat ~sep:""
  |> String.to_array
  |> Array.map ~f:elevation
  |> (fun elevations -> Input { heigth ; width ; elevations })
 
(* Debug functions *)


(* Solution helper functions *)

(* check if the candidate path is shortest; 
   if no path found amonth shortest then candidate is a shortest path *)
let is_shortest_path (candidate: path) (paths: shortest_path list) : bool = 
  let path_elevations: 
  let elevation_index = List.hd_exn candidate in
  let length = List.length candidate in
  let rec check paths:

(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
