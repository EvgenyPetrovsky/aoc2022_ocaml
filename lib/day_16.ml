open Base

type t = Empty
exception Not_implemented
type valve = {
  name: string ; 
  flow_rate: int ;
  leads_to_valves: string list ;
  }

type input = Input of valve list
type answer = Answer of int | Unknown


(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* Parse input functions *)
(* example string:
   Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
*)
let parse_one_line (line:string) : valve =
  let parts = String.split line ~on:' ' in
  let name = List.nth_exn parts 1 in
  let flow_rate = 
    List.nth_exn parts 4 
    |> String.rsplit2_exn ~on:'=' |> snd 
    |> String.chop_suffix_if_exists ~suffix:";"
    |> Int.of_string
  in
  let leads_to_valves = 
    List.drop parts 9 
    |> List.map ~f:(String.chop_suffix_if_exists ~suffix:",") 
  in
  { name ; flow_rate ; leads_to_valves }

let text_to_input (t: string) :input =
  t |> String.split_lines |> List.map ~f:parse_one_line |> (fun x -> Input x)


(* Debug functions *)


(* Solution helper functions *)


(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
