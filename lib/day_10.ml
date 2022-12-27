open Base

type t = Empty
type register = int
type history = register list
type instruction = Noop | Addx of int
exception Not_implemented
exception Invalid_case of string

type input = Input of instruction list
type answer = Answer1 of int | Answer2 of string | Unknown

let parse_one_line (line:string) : instruction =
  match String.split line ~on:' ' with
  | "noop"::_ -> Noop
  | "addx"::value::_ -> Addx (Int.of_string value)
  | _ -> raise (Invalid_case ("for line " ^ line))

let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.map ~f:parse_one_line
  |> (fun x -> Input x)

let process_instructions (ins: instruction list) : history =
  let module Acc = struct
    type t = {v : int ; h : int list}
  end in
  let acc = Acc.({ v = 1 ; h = [1] }) in
  let rec iterate (acc : Acc.t) (ins : instruction list) : Acc.t =
    match ins with
    | [] -> acc
    | Noop::is -> iterate { v = acc.v ; h = acc.v :: acc.h } is
    | (Addx value)::is -> iterate {v = acc.v + value; h = acc.v :: acc.v :: acc.h} is
  in
  let Acc.({h ; _}) = iterate acc ins in
  List.rev h

let debug_history (h: history) : unit =
  Stdio.print_endline "Register history";
  List.iteri h ~f:(fun i x -> Stdio.printf "%d : %d\n" i x)

(* Solution for part 1 *)
let solve_part1 (Input i : input) : answer =
  i
  |> process_instructions
  (* |> (fun x -> debug_history x; x) *)
  |> List.filter_mapi ~f:(fun i x ->
    if (i - 20) % 40 = 0 then Some (x * i) else None)
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer1 x)

(* Solution for part 2 *)
let solve_part2 (Input i : input) : answer =
  i
  |> process_instructions
  (* |> (fun x -> debug_history x; x) *)
  |> (fun x -> List.drop x 1)
  |> List.mapi ~f:(fun i x -> if abs(x - i % 40) <= 1 then '#' else ' ')
  |> List.chunks_of ~length:40
  |> List.map ~f:String.of_char_list
  |> String.concat ~sep:"\n"
  |> (fun x -> Answer2 x)

let answer_to_text = function
  | Answer1 x -> Int.to_string x
  | Answer2 x -> "\n" ^ x
  | Unknown  -> "Solution not yet implemented"


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
