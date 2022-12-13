open Base

exception Not_implemented

type elevation = Start of int | End of int | Elevation of int
type map = { heigth: int ; width: int ; elevations: elevation array }
type rules = { 
  map : map; 
  if_move_is_valid : (elevation -> elevation -> bool) ; (*function that checks if new move in path is valid*)
  if_goal_is_reached : (elevation -> bool) ; (*function that checks of move leads to goal*)
  begin_idx : int (*beginning point on the map*)
  }
type input = Input of map
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
    | 'S' -> Start (Char.to_int 'a' - Char.to_int 'a')
    | 'E' -> End (Char.to_int 'z' - Char.to_int 'a')
    | x -> Elevation (Char.to_int x - Char.to_int 'a')
  in
  lines
  |> String.concat ~sep:""
  |> String.to_array
  |> Array.map ~f:elevation
  |> (fun elevations -> Input { heigth ; width ; elevations })

(* Debug functions *)
let debug_path (Path idxs : path) (map: map)  : unit =
  let idx_coord idx = ("("^Int.to_string(idx / map.width + 1)^","^Int.to_string(idx % map.width + 1)^")") in
  let txt = String.concat ~sep:"-" (List.map (List.rev idxs) ~f:idx_coord) in
  Stdio.printf "%s\n" txt

(* Solution helper functions *)

(* check if the candidate path is shortest;
   if no path found amonth shortest then candidate is a shortest path *)
let is_shortest_path (Path candidate: path) (paths: shortest_path list) : bool =
  let c_map_idx = List.hd_exn candidate in
  let c_length_to_it = List.length candidate in
  let rec check (sps: shortest_path list) : bool =
    match sps with
    | [] -> true
    | {map_idx ; length_to_it } :: sps ->
      if map_idx = c_map_idx then
        if length_to_it > c_length_to_it then true else false
      else check sps
  in
  check paths

let add_path_if_shortest (Path path: path) (shortest_paths: shortest_path list) =
  let new_map_idx = List.hd_exn path in
  let new_length_to_it = List.length path in
  let rec update = function
  | [] -> {map_idx = new_map_idx; length_to_it = new_length_to_it} :: []
  | ({map_idx ; length_to_it} as sp)::sps ->
    if map_idx = new_map_idx && length_to_it > new_length_to_it
      then { sp with length_to_it = new_length_to_it } :: sps
    else if map_idx = new_map_idx
      then sp::sps
    else sp :: (update sps)
  in
  update shortest_paths


let adjacent_positions (idx: int) (m_h: int) (m_w: int) : (int list) =
  [
    if idx % m_w = 0 then -1 else idx - 1 ; (* left *)
    if (idx + 1) % m_w = 0 then -1 else idx + 1 ; (* right *)
    idx + m_w ; (* down *)
    idx - m_w ; (* up *)
  ] |> List.filter ~f:(fun x -> x >= 0 && x < (m_w * m_h))


let discover_new_paths ({ map ; if_move_is_valid ; _} : rules) (discovered: path list) : (path list) =
  let { heigth ; width ; elevations } = map in
  let add_adj (Path p: path) : (path list) =
    let idx = List.hd_exn p in
    let e0 = Array.get elevations idx in
    adjacent_positions idx heigth width
    |> List.filter ~f:(fun x -> let e1 = Array.get elevations x in if_move_is_valid e0 e1)
    |> List.map ~f:(fun x -> Path (x :: p))
  in
  List.fold discovered ~init:[] ~f:(fun z x -> List.append z (add_adj x))


let filter_new_paths_and_update_shortest (nps: path list) (sps: shortest_path list) : ((path list) * (shortest_path list))=
  let rec iter acc_np nps sps =
    match nps with
    | [] -> (acc_np, sps)
    | np::nps ->
      let is_shortest = is_shortest_path np sps in
      let new_sps = add_path_if_shortest np sps in
      if is_shortest then iter (np::acc_np) nps new_sps else iter acc_np nps new_sps
  in
  iter [] nps sps


let discover_shortest_paths ({map ; if_goal_is_reached ; begin_idx ; _} as rules : rules) : path list =
  let {elevations ; _} = map in
  let init_path = Path [begin_idx] in
  let rec iter (paths_to_goal: path list) (paths: path list) (shortest_paths: shortest_path list) : (path list) =
    let new_paths = discover_new_paths rules paths in
    let (new_paths_filtered, new_shortest_paths) = filter_new_paths_and_update_shortest new_paths shortest_paths in
    let terminal_state = List.is_empty new_paths_filtered in
    let new_paths_to_goal = List.append paths_to_goal @@ List.filter new_paths_filtered ~f:(fun (Path x) -> if_goal_is_reached @@ Array.get elevations (List.hd_exn x) ) in
    (*
    Stdio.printf "terminal state: %s\n" (Bool.to_string terminal_state);
    Stdio.printf "number of paths: %d\n" (List.length new_paths_filtered);
    List.iter new_paths_filtered ~f:(fun x -> debug_path x map);
    *)
    if terminal_state then new_paths_to_goal else iter new_paths_to_goal new_paths_filtered new_shortest_paths
  in
  let paths = iter [] [init_path] [] in
  paths


(* Solution for part 1 *)
let part1 (Input ({width ; heigth ; elevations} as map): input) : answer =
  let if_move_is_valid : (elevation -> elevation -> bool) = 
    fun e1 e2 -> 
      match e1 with
      | End _ -> false
      | Start v1 | Elevation v1 -> 
        match e2 with 
        | Start _ -> false 
        | End v2 | Elevation v2 -> v2 - v1 < 2
  in
  let if_goal_is_reached : (elevation -> bool) =
    fun e -> match e with | End _ -> true | _ -> false
  in
  let (begin_idx, _) = Array.findi_exn elevations ~f:(fun _ x -> match x with | Start _ -> true | _ -> false) in
  let rules = ({ map ; if_move_is_valid ; if_goal_is_reached ; begin_idx} : rules) in
  let paths = discover_shortest_paths rules in
  paths
  |> List.map ~f:(fun (Path x) -> List.length x)
  |> List.fold ~init:(width * heigth) ~f:min
  |> (fun x -> Answer (x - 1))


(* Solution for part 2 *)
let part2 (Input ({width ; heigth ; elevations} as map) : input) : answer =
let if_move_is_valid : (elevation -> elevation -> bool) = 
  fun e2 e1 ->
    match e2 with
    | Elevation 0 | Start _ -> false
    | End v2 | Elevation v2 -> 
      match e1 with 
      | End _ -> false
      | Start v1 | Elevation v1 -> v2 - v1 < 2
in
let if_goal_is_reached : (elevation -> bool) =
  fun e -> match e with | Elevation 0 | Start _ -> true | _ -> false
in
let (begin_idx, _) = Array.findi_exn elevations ~f:(fun _ x -> match x with | End _ -> true | _ -> false) in
let rules = ({ map ; if_move_is_valid ; if_goal_is_reached ; begin_idx} : rules) in
let paths = discover_shortest_paths rules in
paths
|> List.map ~f:(fun (Path x) -> List.length x)
|> List.fold ~init:(width * heigth) ~f:min
|> (fun x -> Answer (x - 1))
