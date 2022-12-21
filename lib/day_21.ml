open Base

exception Not_implemented

type expression =
| Lit of int
| Add of expression * expression
| Sub of expression * expression
| Mul of expression * expression
| Div of expression * expression

let expressions_text_tree = Map.empty (module String)

type input = Input of expression
type answer = Answer of int | Unknown


(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* Parse input functions *)
(* just split line into monkey name and what it does *)
let text_line_to_tupple (line:string): (string * string) = 
  let re_top = Str.regexp {|\([a-z]+\): \(.*\)|} in
  let monkey_name = Str.replace_first re_top {|\1|} line in
  let monkey_job = Str.replace_first re_top {|\2|} line in
  (monkey_name, monkey_job)

(*
let draft unit : unit =
  if List.exists ['+','-','*','/'] ~f:(fun c -> String.contains monkey_job c)
    then String.split ~on:' ' monkey_job |> List.filter ~f:(fun x -> not @@ List.is_empty x)
    let re_exp = Str.regexp {|\([a-z]+\) [+-\*/] \([a-z]+\)|} in
    let re_num = Str.regexp {|\([0-9]+\)|} in
    let Str.
*)

let parse_map (map: (string * string) map) : expression =
  let iter (monkey:string) : expression = 
    let job_text = Map.find map monkey in
    let job = match String.split ~on:' ' job in
    | [x] -> Int Int.of_string x
    | l::o::r::[] -> 
      let (lj, rj) = (iter l, iter r) in 
      match o with 
      | "+" -> Add lj rj
      | "-" -> Sub lj rj
      | "*" -> Mul lj rj
      | "/" -> Div lj rj
      | _ -> raise Invalid_case
    | _ -> raise Invalid_case
  in
  iter "root"


let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.map ~f:text_line_to_tupple
  |> fun x -> Map.of_alist_exn (module String) x
  |> parse_map
  |> fun x -> Input x


(* Debug functions *)


(* Solution helper functions *)


(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
