open Base

exception Not_implemented
exception Not_expected_case


type packet = | PInt of int * packet | PLst of packet * packet | PNul
type pair = { left : packet ; right : packet }
type input = Input of pair list
type answer = Answer of int | Unknown

(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* Parse input functions *)
let parse_one_line (s: string) : packet =

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

  let rec iter (rem: char list) : packet =
    match rem with
    | [] -> PNul
    | ']'::[] -> PNul
    | ','::rem -> iter rem
    | '['::']'::rem -> PLst (PNul, iter rem) (*just empty group*)
    | '['::rem -> let (g, rem) = read_group_part ('['::rem) in PLst (iter g, iter rem)
    | rem ->
      let (num, rem) = read_number_part rem in
      let num = Int.of_string @@ String.of_char_list num in
      PInt (num , iter rem)
  in
  iter @@ String.to_list s

let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.group ~break:(fun x _ -> String.equal x "")
  |> List.map ~f:(fun x -> {left = parse_one_line (List.nth_exn x 0); right = parse_one_line (List.nth_exn x 1)})
  |> (fun x -> Input x)


(* Debug functions *)


(* Solution helper functions *)

let rec p_compare (left: packet) (right: packet) : int =
  match (left, right) with
  | (PInt (l, ls) , PInt (r, rs)) -> if l < r then -1 else if l > r then +1 else p_compare ls rs
  | (PLst (l, ls) , PLst (r, rs)) -> let res = p_compare l r in if res = 0 then p_compare ls rs else res
  | (PNul, PInt _) | (PNul, PLst _) -> -1
  | (PInt _, PNul) | (PLst _ , PNul) -> 1
  | (PInt (l, ls) , PLst (r, rs)) -> let res = p_compare (PInt (l , PNul)) r in if res = 0 then p_compare ls rs else res
  | (PLst (l, ls) , PInt (r, rs)) -> let res = p_compare l (PInt (r , PNul)) in if res = 0 then p_compare ls rs else res
  | (PNul, PNul) -> 0

(* Solution for part 1 *)
let part1 (Input i : input) : answer =
  i
  |> List.map ~f:(fun {left ; right} -> p_compare left right)
  |> List.filter_mapi ~f:(fun i x -> match x with | -1 -> Some (i + 1) | _ -> None)
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer x)


(* Solution for part 2 *)
let part2 (Input i : input) : answer =
  let dp2 = PLst (PLst (PInt (2, PNul), PNul), PNul) in
  let dp6 = PLst (PLst (PInt (6, PNul), PNul), PNul) in
  let incl_dp = {left = dp2 ; right = dp6} :: i in
  incl_dp
  |> List.concat_map ~f:(fun {left ; right} -> [left ; right])
  |> List.sort ~compare:p_compare
  |> List.filter_mapi ~f:(fun i x -> if p_compare x dp2 = 0 || p_compare x dp6 = 0 then Some (i + 1) else None)
  |> List.fold ~init:1 ~f:( * )
  |> (fun x -> Answer x)

