open Base

type input = Input of char list
type answer = Answer of int | Unknown
type marker = Packet | Message

let text_to_input (t: string) :input =
  let chars = String.to_list t in
  Input chars

let identify_start_of (m:marker) (cs: char list) =
  let len = match m with | Packet -> 4 | Message -> 14 in
  let rec identify len cs acc =
    let (beg, _) = List.split_n cs len in
    let unq_len = List.dedup_and_sort ~compare:(Char.compare) beg in
    if List.length unq_len = 0 then 0
    else if List.length unq_len = len then acc + len
    else identify len (List.drop cs 1) (acc + 1)
  in
  identify len cs 0

(* Solution for part 1 *)
let solve_part1 (Input i : input) : answer =
  Answer (identify_start_of Packet i)

(* Solution for part 1 *)
let solve_part2 (Input i : input) : answer =
  Answer (identify_start_of Message i)

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
