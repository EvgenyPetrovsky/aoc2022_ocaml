open Base

type t = Empty
exception Not_implemented
exception Invalid_case

(*introduce range type for those x1,x2 and y1,y1*)
type range = | X of (int*int) | Y of (int*int)
type coordinate = int * int
type sensor = { position: coordinate ; closest_beacon: coordinate }

type input = Input of sensor list
type answer = Answer of int | Unknown


(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


  (* Parse input functions *)
let text_to_input (_: string) :input =
  raise Not_implemented


(* Debug functions *)


(* Solution helper functions *)
(*calculate MHT distance*)
let mht_distance (c1: coordinate) (c2: coordinate) : int =
  let (x1,y1) = c1 in 
  let (x2,y2) = c2 in
  abs(x1-x2) + abs(y1-y2)

(*return pair of x1..x2 coordinated for given center and MHT distance and y *)
let sensor_h_coverage ~(y: int) (sensor: sensor) : range option =
  let mht_dist = mht_distance sensor.position sensor.closest_beacon in
  let (x0, y0) = sensor.position in 
  let abs_dy = abs(y0-y) in 
  let xr = mht_dist - abs_dy in 
  if xr < 0 then None else Some (X (x0 - xr, x0 + xr))

(*do the same for y1..y1*)
let sensor_v_coverage ~(x: int) (sensor: sensor) : range option =
  let mht_dist = mht_distance sensor.position sensor.closest_beacon in
  let (x0, y0) = sensor.position in 
  let abs_dx = abs(x0-x) in 
  let yr = mht_dist - abs_dx in 
  if yr < 0 then None else Some (Y (y0 - yr, y0 + yr))
  
(*introduce operations on ranges: UNION and intersect*)
(* merge unites two ranges only if they are overlapping *)
let merge_ranges r1 r2 =
  match (r1, r2) with
  | (X (l1, r1), X(l2, r2)) -> if r1 >= l2 then Some (X (l1, r2)) else None
  | (Y (d1, u1), Y(d2, u2)) -> if u1 >= d2 then Some (X (d1, u2)) else None
  | _ -> raise Invalid_case

let intersect_ranges r1 r2 =
  match (r1, r2) with
  | (X (l1, r1), X(l2, r2)) -> if r1 >= l2 then Some (X (max l1 l2, min r1 r2)) else None
  | (Y (d1, u1), Y(d2, u2)) -> if u1 >= d2 then Some (X (max d1 d2, min u1 u2)) else None
  | _ -> raise Invalid_case

let range_low_boundary = function | X (l, _) -> l | Y (d, _) -> d
let range_length = function | X (l, r) -> r - l + 1 | Y (d, u) -> u - d + 1

(* OR TO MAKE IT EASIER: 
   find out min x1 and max x2 and validate every value for belonging to range *)
let find_h_boudaries ~(y:int) (sensors: sensor list) : range =
  let xs = List.filter_map sensors ~f:(fun x -> sensor_h_coverage ~y:y x) in
  let x_min = xs |> List.map ~f:range_low_boundary |> List.min_elt ~compare:Int.compare in 
  let x_max = xs |> List.map ~f:range_low_boundary |> List.max_elt ~compare:Int.compare in 
  match (x_min, x_max) with 
  | (Some left, Some right) -> X (left, right)
  | _ -> raise Invalid_case

let diff_range r1 r2 =
  let ((a1,b1), (a2,b2)) = (r1, r2) in
  if a2 < b1 then None
  else if a1 <= a2 && b2 <= b1 then Some (a2, b2)
  else if a1 <= a2 && b1 <= b2 then Some (a2, b1)
  else if a2 <= a1 && b1 <= b2 then Some (a1, b1)
  else if a2 <= a1 && b2 <= b1 then Some (a1, b2)
  else None

    else 
  match (r1, r2) with
  | (X (l1, r1), X (l1, r1) -> 

(* Solution for part 1 *)
let part1 (Input sensors : input) : answer = 
  (*let y = 2000000 in*)
  let y = 10 in
  let sensor_cov_ranges = List.filter_map sensors ~f:(fun x -> sensor_h_coverage ~y:y x) in
  let global_boundaries = find_h_boudaries ~y:y sensors in
  let global_low_x_bound = range_low_boundary global_boundaries in 
  let global_range_length = range_length global_boundaries in
  let coverage_array = Array.init global_range_length ~f:(fun _ -> false) in
  let mark_local_range (array: bool array) (rng: range) : (bool array) =
    let rng_low_bound = range_low_boundary rng in
    let rng_len = range_length rng in
    let module A = Array in
    A.init rng_len ~f:(fun x -> x) |> A.iter ~f:(
      fun x -> A.set array (x + rng_low_bound - global_low_x_bound) true);
    array
  in 
  sensor_cov_ranges
  |> List.fold ~init:coverage_array ~f:mark_local_range
  |> Array.count ~f:(fun x -> x)
  |> (fun x -> Answer x)

(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
