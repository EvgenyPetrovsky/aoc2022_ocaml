open Base

exception Not_implemented
exception Invalid_case

type input = Input of string list
type answer = Answer of string | Unknown
let base = 5


(* Answer print function *)
let answer_to_text = function
  | Answer x -> x
  | Unknown  -> "Solution not yet implemented"


  (* Parse input functions *)
let text_to_input (t: string) :input =
  Input (String.split_lines t)


(* Debug functions *)


(* Solution helper functions *)
let decode (snafu: string) : int =
  let sym_to_dec = function
  | '2' -> 2
  | '1' -> 1
  | '0' -> 0
  | '-' -> -1
  | '=' -> -2
  | _ -> raise Invalid_case
  in
  snafu
  |> String.to_list
  |> List.map ~f:sym_to_dec
  |> List.fold_left ~init:0 ~f:(fun z x -> z * 5  + x)


let encode (decimal: int) : string =
  let dec_to_sym = function
  | 2  -> '2'
  | 1  -> '1'
  | 0  -> '0'
  | -1 -> '-'
  | -2 -> '='
  | _ -> raise Invalid_case
  in
  (* function that converts number from base 10 to list of base 5 *)
  let rec iter_dec (acc : int list) (dec : int) : int list =
    if dec < base then dec :: acc
    else iter_dec ((dec % base)::acc) ((dec / base))
  in
  let base_5_normal = iter_dec [] decimal in
  (* function that turns normal number of base 5 into snafu (range -2..2) *)
  let process_one_digit (modifier:int) (this:int) : (int * int) =
    (* adding the accummulated modification to current number *)
    let new_num = modifier + this in
    (* trasnform number into current order digit and higher order digit *)
    let (new_div, new_mod) = (new_num / base , new_num % base) in
    if new_mod > 2 then ((new_div+1) , (new_mod - base))
    else (new_div , new_mod)
  in
  base_5_normal
  (* add leading zeroes for propagation of digist to higher orders
     e.g. 3 is "1=" *)
  |> fun right -> List.append (List.init 100 ~f:(fun _ -> 0)) right
  (* we need to apply following step to reversed list (start from rightmost element) *)
  |> List.rev
  (* produces a list using accumulator and list *)
  |> List.folding_map ~init:0 ~f:process_one_digit
  (* reverse the list back *)
  |> List.rev
  (* remove all leading zeros that we have added for safety *)
  |> List.drop_while ~f:((=) 0)
  |> List.map ~f:dec_to_sym
  |> String.of_char_list


(* Solution for part 1 *)
let part1 (Input snafu_numbers : input) : answer =
  snafu_numbers
  |> List.map ~f:decode
  |> List.fold ~init:0 ~f:(+)
  |> encode
  |> fun x -> Answer x


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
