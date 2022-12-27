open Base

type t = Empty
exception Not_implemented

type input = Input of t
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


(* Solution for part 1 *)
let solve_part1 (Input _ : input) : answer = Unknown


(* Solution for part 2 *)
let solve_part2 (Input _ : input) : answer = Unknown


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
