open Base

exception Not_implemented

type elevation = Start of int | End of int | Elevation of int
type map = { heigth: int ; width: int ; elevations: elevation array }
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
let discover_new_paths ({heigth ; width ; elevations} : map) (discovered: path list) : (path list) =
  let add_adj (Path p: path) : (path list) =
    let idx = (List.hd_exn p) in
    match Array.get elevations idx with
    | End _ -> [Path p]
    | Start e0 | Elevation e0 ->
      adjacent_positions idx heigth width
      |> List.filter ~f:(fun x ->
        match Array.get elevations x with
        End e1 | Elevation e1 -> (e1 - e0) <= 1
        | _ -> false)
      |> List.map ~f:(fun x -> Path (x :: p))
  in
  List.fold discovered ~init:[] ~f:(fun z x -> List.append z (add_adj x))


(* due to non working map (why???) custom implementation of list comparison *)
let list_compare f l1 l2 =
  let rec iter x y =
    match (x, y) with
    | ([], []) -> 0
    | (_::_, []) -> 1
    | ([], _::_) -> -1
    | (x::xs, y::ys) -> match (f x y) with | 0 -> iter xs ys | _ -> (f x y)
  in
  iter l1 l2


(* check if any progress was made after the discovery step *)
let any_progress_made (before: path list) (after: path list) : bool =
  let comp_fun = fun (Path x) (Path y) -> list_compare Int.compare x y in
  let l0 = List.dedup_and_sort before ~compare:comp_fun in
  let l1 = List.dedup_and_sort after  ~compare:comp_fun in
  List.equal (fun x y -> comp_fun x y = 0) l0 l1


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

let discover_shortest_paths ({elevations ; _} as map: map) : path list =
  let start_idx_array = Array.filter_mapi elevations ~f:(fun i x -> match x with | Start _ -> Some i | _ -> None) in
  let init_path = Path [Array.get start_idx_array 0] in
  let rec iter (paths_to_goal: path list) (paths: path list) (shortest_paths: shortest_path list) : (path list) =
    let new_paths = discover_new_paths map paths in
    (*
    let new_shortest_paths = List.fold new_paths ~init:shortest_paths ~f:(fun z x -> add_path_if_shortest x z) in
    let new_paths_filtered = List.filter new_paths ~f:(fun x -> is_shortest_path x new_shortest_paths) in
    *)
    let (new_paths_filtered, new_shortest_paths) = filter_new_paths_and_update_shortest new_paths shortest_paths in
    let terminal_state = List.is_empty new_paths_filtered in
    let new_paths_to_goal = List.append paths_to_goal @@ List.filter new_paths_filtered ~f:(fun (Path x) -> match Array.get elevations (List.hd_exn x) with | End _ -> true | _ -> false) in
    Stdio.printf "terminal state: %s\n" (Bool.to_string terminal_state);
    Stdio.printf "number of paths: %d\n" (List.length new_paths_filtered);
    List.iter new_paths_filtered ~f:(fun x -> debug_path x map);
    if terminal_state then new_paths_to_goal else iter new_paths_to_goal new_paths_filtered new_shortest_paths
  in
  let paths = iter [] [init_path] [] in
  paths


(* Solution for part 1 *)
let part1 (Input ({width ; heigth ; elevations} as map): input) : answer =
  let paths = discover_shortest_paths map in
  paths
  |> List.filter ~f:(fun (Path x) -> match Array.get elevations (List.hd_exn x) with | End _ -> true | _ -> false)
  |> List.map ~f:(fun (Path x) -> List.length x)
  |> List.fold ~init:(width * heigth) ~f:min
  |> (fun x -> Answer (x - 1))


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
