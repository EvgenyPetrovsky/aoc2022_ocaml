open Base

type t = Empty
exception Not_implemented

type input = Input of int array
type answer = Answer of int | Unknown


(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


  (* Parse input functions *)
let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.map ~f:Int.of_string
  |> List.to_array
  |> (fun x -> Input x)


(* Debug functions *)


(* Solution helper functions *)
let locate_number (number:int) (array: int array) : int =
  let (idx, _) = Array.findi_exn array ~f:(fun _ x -> x = number) in
  idx

let mix_array ~(pointer: int) ~(shift_by: int) ~(pointers: int array) : int array =
  let len = Array.length pointers in
  let offset = locate_number pointer pointers in
  (* calculate new position in array *)
  (* impotant to remember that number of positions is 1 less than length of array
     because of moving element is owning this one position *)
  let move = abs(shift_by) % (len - 1) in
  let new_pos =
    if shift_by < 0 && (offset - move) < 0
      then (offset - move) % (len - 1)
    else if shift_by > 0 && (offset + move) >= len
      then (offset + move) % (len - 1)
    else if shift_by < 0
      then offset - move
    else offset + move
  in
  (* decide where to move from current position - left:-1; right:+1 *)
  let sign = if new_pos = offset then 0 else (new_pos - offset) / abs(new_pos - offset) in
  (* remainder of division to save on unnecessary multiple rotations *)
  for i = 0 to abs (new_pos - offset) - 1 do
    (* find indexes of elements that need to be swapped *)
    let this_idx = (offset + (i * sign)) in
    let next_idx = (offset + ((i + 1) * sign)) in
    (* swap elements *)
    let temp = pointers.(next_idx) in
    pointers.(next_idx) <- pointers.(this_idx);
    pointers.(this_idx) <- temp
  done;
  pointers

(* Solution for part 1 *)
let part1 (Input numbers : input) : answer =
  let len = Array.length numbers in
  let pointers = Array.init len ~f:(fun i -> i) in
  let pointers = Array.foldi numbers ~init:pointers ~f:(fun i pointers number ->
    mix_array ~pointer:i ~shift_by:number ~pointers:pointers)
  in
  let new_order = Array.map pointers ~f:(fun ptr -> numbers.(ptr)) in
  let idx_0 = locate_number 0 new_order in
  [1000;2000;3000]
  |> List.map ~f:(fun x -> x + idx_0)
  |> List.fold ~init:0 ~f:(fun z x -> z + new_order.(x % len))
  |> (fun x -> Answer x)


(* Solution for part 2 *)
let part2 (Input numbers : input) : answer =
  let len = Array.length numbers in
  let decription_key = 811589153 in
  let numbers = Array.map numbers ~f:(fun x -> x * decription_key) in
  let numbers = List.init 10 ~f:(fun _ -> numbers) |> Array.concat in
  let pointers = Array.init len ~f:(fun i -> i) in
  let pointers = Array.foldi numbers ~init:pointers ~f:(fun i pointers number ->
    mix_array ~pointer:(i%len) ~shift_by:number ~pointers:pointers)
  in
  let new_order = Array.map pointers ~f:(fun ptr -> numbers.(ptr)) in
  let idx_0 = locate_number 0 new_order in
  [1000;2000;3000]
  |> List.map ~f:(fun x -> x + idx_0)
  |> List.fold ~init:0 ~f:(fun z x -> z + new_order.(x % len))
  |> (fun x -> Answer x)
