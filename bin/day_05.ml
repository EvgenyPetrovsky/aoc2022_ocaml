open Base

type t

type input = Input of t
type answer = Answer of int | Unknown

let text_to_input (t: string) :input = Input Empty

(* Solution for part 1 *)
let part1 (Input i : input) : answer = Unknown

(* Solution for part 1 *)
let part2 (Input i : input) : answer = Unknown

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"
