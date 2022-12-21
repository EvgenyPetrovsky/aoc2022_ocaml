open Base

exception Not_implemented
exception Invalid_case

type expression =
| Literal of string * int
| Plus of string * expression * expression
| Minus of string * expression * expression
| Multiply of string * expression * expression
| Divide of string * expression * expression

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

let parse_map (map : (string, 'a, 'b) Map.t) : expression =
  let rec iter (monkey:string) : expression =
    let job_text = Map.find_exn map monkey in
    match (String.split ~on:' ' job_text) with
    | n::[] -> Literal (monkey, Int.of_string n)
    | l::"+"::r::[] -> Plus (monkey, iter l , iter r)
    | l::"-"::r::[] -> Minus (monkey, iter l , iter r)
    | l::"*"::r::[] -> Multiply (monkey, iter l , iter r)
    | l::"/"::r::[] -> Divide (monkey, iter l , iter r)
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
let rec evaluate (expr: expression) : int =
  match expr with
  | Literal (_, x) -> x
  | Plus (_, l, r) -> (evaluate l) + (evaluate r)
  | Minus (_, l, r) -> (evaluate l) - (evaluate r)
  | Multiply (_, l, r) -> (evaluate l) * (evaluate r)
  | Divide (_, l, r) -> (evaluate l) / (evaluate r)

let set_humn ~(value: int) (expr: expression) : expression =
  let rec iter = function
  | Literal ("humn", _) -> Literal ("humn", value)
  | Literal (n, v) -> Literal (n, v)
  | Plus (n, l, r) -> Plus(n, iter l, iter r)
  | Minus (n, l, r) -> Minus(n, iter l, iter r)
  | Multiply (n, l, r) -> Multiply(n, iter l, iter r)
  | Divide (n, l, r) -> Divide(n, iter l, iter r)
  in
  iter expr

let redefine_root (expr: expression) : expression =
  match expr with
  | Plus ("root", l, r) -> Minus("root", l, r)
  | _ -> raise Invalid_case


(* this function optimizes "humn" input to yield 0 difference
   assuming that we have one local minimum so moving away from it
   will cause difference to grow *)
let optimize (expr: expression) : int =
  let delta = 16 in
  let step_limit = 1000 in
  let par0 = 0 in
  let expr1 = set_humn ~value:par0 expr in
  let res0 = evaluate expr1 in
  let rec iter (delta: int) (par0:int) (res0: int) (step_number:int) : int =
    let step_number = step_number + 1 in
    let par1 = delta + par0 in
    let expr1 = set_humn ~value:par1 expr in
    let res1 = evaluate expr1 in
    Stdio.printf "Step %d: par0: %d , delta: %d , par1: %d , res0: %d , res1: %d .\n" step_number par0 delta par1 res0 res1;

    (* if local minimum found and numbers on root level are equal: we found solution *)
    if res1 = 0 then par1
    (* ifwe ran out of number of attampts we stop *)
    else if delta = 0 then 0
    (* if we ran out of number of attampts we stop *)
    else if step_number > step_limit then 0
    (* if we crossed local minimum we try again and make two times shorter step *)
    else if res1 < 0 && 0 < res0 || res1 > 0 && 0 > res0
      then iter (delta/2) par0 res0 step_number
    (* if we are getting farther from local minimum then we reject the move and turn around *)
    else if res1 < res0 && res0 < 0 || 0 < res0 && res0 < res1
      then iter (-delta) par0 res0 (step_number)
    (* if we are moving in right direction we make 2 time longer steps *)
    else if res0 < res1 && res1 < 0 || 0 < res1 && res1 < res0
      then iter (delta*2) par1 res1 (step_number)
    (* cant think of another case - it is netter to fail for safety *)
    else raise Invalid_case
  in
  iter delta par0 res0 0

(* Solution for part 1 *)
let part1 (Input expr : input) : answer =
  Answer (evaluate expr)


(* Solution for part 2 *)
let part2 (Input expr : input) : answer =
  let new_expr =  redefine_root expr in
  Answer (optimize new_expr)
