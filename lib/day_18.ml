open Base

type t = Empty
exception Not_implemented
exception Invalid_case

type lava_unit = int * int * int
type lava_droplet = { surface: int ; units : lava_unit list }

type input = Input of lava_unit list
type answer = Answer of int | Unknown

(* coordinate modifiers *)
let adjacent = [
  (-1, 0, 0) ;
  ( 1, 0, 0) ;
  ( 0,-1, 0) ;
  ( 0, 1, 0) ;
  ( 0, 0,-1) ;
  ( 0, 0, 1) ;
]

(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


  (* Parse input functions *)
let parse_one_line (line: string) : lava_unit =
  let lst = String.split line ~on:',' in
  match lst with
  | x :: y :: z :: _ -> (Int.of_string x, Int.of_string y, Int.of_string z)
  | _ -> raise Invalid_case
let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.map ~f:parse_one_line
  |> (fun x -> Input x)


(* Debug functions *)


(* Solution helper functions *)

(* add one unit into droplet *)
let grow_droplet ({ surface ; units }: lava_droplet) (unit: lava_unit) :lava_droplet =
  let (x, y, z) = unit in
  let adj_coord = List.map adjacent ~f:(fun (dx, dy, dz) -> (x+dx, y+dy, z+dz)) in
  let adj_count = List.count adj_coord ~f:(
    fun (ax, ay, az) -> List.exists units ~f:(
      fun (ux, uy, uz) -> ax = ux && ay = uy && az = uz
    )
  ) in
  (* new surface of droplet is
     6 new facets of unit
     minus adjucent facets of unit
     minus adjacent facets of droplet *)
  let new_surface = surface + 6 - (2 * adj_count) in
  { surface = new_surface ; units = unit :: units }

(* This function will
   scan area about droplet and find all coordinates
   that are outside of droplet and inside min/max boundaries *)
let discover_outside_units (min_x, min_y, min_z) (max_x, max_y, max_z) (lava_units : lava_unit list) : (int * int * int ) list =
  let valid_adj_point ((x,y,z) : int * int * int) (discovered: (int * int * int) list) : bool =
    if x < min_x || y < min_y || z < min_z then false
    else if x > max_x || y > max_y || z > max_z then false
    else if List.exists discovered ~f:(fun(xd,yd,zd) -> xd = x && yd = y && zd = z) then false
    else if List.exists lava_units ~f:(fun(xl,yl,zl) -> xl = x && yl = y && zl = z) then false
    else true
  in
  let rec iter (acc : (int * int * int) list) ((x,y,z): int * int * int) : (int * int * int) list =
    let new_adjacent = adjacent
    |> List.map ~f:(fun (dx, dy, dz) -> (x + dx, y + dy, z + dz))
    |> List.filter ~f:(fun p -> valid_adj_point p acc)
    in
    List.fold new_adjacent ~init:(List.append new_adjacent acc) ~f:iter
  in
  iter [] (0,0,0)

(* Solution for part 1 *)
let solve_part1 (Input units : input) : answer =
  let init_droplet = { surface = 0 ; units = [] } in
  let final_droplet = List.fold units ~init:init_droplet ~f:grow_droplet in
  Answer (final_droplet.surface)


(* Solution for part 2 *)
(* approach
   - to simulate droplet out of units surrounding it within its boundaries
   - snubstract well known outside area of "cubic surface from total area "*)
let solve_part2 (Input units : input) : answer =
  let get_max (extract: lava_unit -> int) : int =
    units |> List.map ~f:extract |> List.max_elt ~compare:Int.compare |> function | Some x -> x | _ -> raise Invalid_case
  in
  let get_min (extract: lava_unit -> int) : int =
    units |> List.map ~f:extract |> List.min_elt ~compare:Int.compare |> function | Some x -> x | _ -> raise Invalid_case
  in
  (* find boundaries of droplet
     and expand it by 1 to create some space around
     for discovery algorithm to fully cover droplet from all outer sides *)
  let min_x = get_min (fun (x,_,_) -> x) - 1 in
  let max_x = get_max (fun (x,_,_) -> x) + 1 in
  let min_y = get_min (fun (_,y,_) -> y) - 1 in
  let max_y = get_max (fun (_,y,_) -> y) + 1 in
  let min_z = get_min (fun (_,_,z) -> z) - 1 in
  let max_z = get_max (fun (_,_,z) -> z) + 1 in
  (* calculate the area of surface of boundaries *)
  let outside_cube_surface =
    (max_x-min_x + 1) * (max_y-min_y + 1) * 2 +
    (max_y-min_y + 1) * (max_z-min_z + 1) * 2 +
    (max_z-min_z + 1) * (max_x-min_x + 1) * 2
  in
  (* explore the outside space around droplet within min/max boundaries *)
  let outside_units = discover_outside_units (min_x,min_y,min_z) (max_x,max_y,max_z) units in
  (* calculate the surface for outsede *)
  let outside_units_total_surface = outside_units
  |> List.fold ~init:{surface = 0 ; units = []} ~f:grow_droplet
  |> (fun x -> x.surface)
  in
  (*
  Stdio.printf "Cube coordinates:\n";
  List.iteri outside_units ~f:(fun i (x, y, z) -> Stdio.printf "( %d , %d , %d ) -- i %d\n" x y z i);
  Stdio.printf "Min x %d, Max x %d, Min y %d, Max y %d, Min z %d, Max z %d\n" min_x max_x min_y max_y min_z max_z;
  Stdio.printf "Cube outside and inside surface %d\n" outside_units_total_surface;
  *)
  Answer(outside_units_total_surface - outside_cube_surface)


  (* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
