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
  (* figure ouy why numenr of steps must be 1 less fr negative shifts *)
  let moves = if shift_by < 0 then shift_by % len -1 else shift_by % len in
  (* remainder of division to save on unnecessary multiple rotations *)
  for i = 0 to moves - 1 do
    (* find indexes of elements that need to be swapped *)
    let this_idx = (i + offset) % len in
    let next_idx = (i + offset + 1) % len in
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
let part2 (Input _ : input) : answer = Unknown
