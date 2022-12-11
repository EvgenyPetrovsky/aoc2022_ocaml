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
let text_to_input (t: string) :input =
  raise Not_implemented


(* Debug functions *)


(* Solution helper functions *)


(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
