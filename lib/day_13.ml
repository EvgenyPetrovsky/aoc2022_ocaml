open Base

exception Not_implemented
exception Not_expected_case


type hierarchy = | L of int | G of (hierarchy list)
type pair = { left : (hierarchy list) ; right : (hierarchy list) }
type input = Input of pair list
type answer = Answer of int | Unknown

(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* Parse input functions *)
let parse_one_line (s: string) : (hierarchy list) = 

  let rec read_number_part (from_str:char list) : (char list) * (char list) =
    match from_str with
    | [] -> ([], [])
    | c::cs -> 
      if (Char.(>=) c '0' && Char.(<=) c '9') 
        then let (a, b) = read_number_part cs in (c::a, b)
      else ([] , c::cs)
  in    
  let read_group_part (from_str:char list) : (char list) * (char list) =
    let rec scan lev from_str = 
      match (lev, from_str) with
      | (0, '['::cs) -> let (a, b) = scan 1 cs in (a, b)
      | (1, ']'::cs) -> ([], cs)
      | (l, '['::cs) -> let (a, b) = scan (l + 1) cs in ('['::a, b)
      | (l, ']'::cs) -> let (a, b) = scan (l - 1) cs in (']'::a, b)
      | (l, c::cs) -> let (a, b) = scan l cs in (c::a, b)
      | _ -> raise (Not_expected_case)
    in
    scan 0 from_str
  in
    
  let rec iter (rem: char list) : (hierarchy list) =
    match rem with
    | [] -> []
    | ']'::[] -> []
    | ','::rem -> iter rem
    | '['::']'::rem -> (G [])::(iter rem) (*just empty group*)
    | '['::rem -> let (g, rem) = read_group_part ('['::rem) in (G (iter g)) :: (iter rem)
    | rem -> 
      let (num, rem) = read_number_part rem in 
      let num = Int.of_string @@ String.of_char_list num in
      (L num) :: iter rem
  in
  iter @@ String.to_list s

let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.group ~break:(fun x _ -> String.equal x "")
  |> List.map ~f:(fun x -> {left = parse_one_line (List.nth_exn x 0); right = parse_one_line (List.nth_exn x 0)})
  |> (fun x -> Input x)


(* Debug functions *)


(* Solution helper functions *)


(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
