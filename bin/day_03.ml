open Base

type t = Undefined

type item = char
type compartment = item list
type rucksack = {one: compartment; two: compartment}
type input = Input of rucksack list
type answer = Answer of int | Unknown

let text_to_input (t: string) : input =
  let to_compartments (is: item list) : rucksack =
    let (one, two) = List.split_n is (List.length is / 2) in {one; two}
  in
  t
  |> String.split_lines
  |> List.map ~f:String.to_list
  |> List.map ~f:to_compartments
  |> (fun x -> Input x)

let find_item_in_both {one; two} =
  let what_i_found =
    List.filter one ~f:(fun x -> List.exists two ~f:(Char.equal x))
  in
  match what_i_found with
  | [] -> '0'
  | hd :: _ -> hd

let item_priority (i:item) : int =
  let upper = String.to_array "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let lower = String.to_array "abcdefghijklmnopqrstuvwxyz" in
  let prio (ls:item array) (l:item) : int =
    match Array.findi ls ~f:(fun _ elt -> Char.equal l elt) with
    | None -> 0
    | Some (index, _) -> index + if Char.is_uppercase l then 27 else 1
  in
  (prio upper i) + (prio lower i)

(* Solution for part 1 *)
let part1 (Input i : input) : answer =
  i
  |> List.map ~f:(find_item_in_both)
  |> List.map ~f:(item_priority)
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer x)

let find_item_in_many (items: item list list) : item =
  let (hd, tl) = (List.hd_exn items, List.tl_exn items) in
  let common_items this that =
    List.filter this ~f:(fun x -> List.exists that ~f:(Char.equal x))
  in
  let what_i_found = List.fold tl ~init:hd ~f:(common_items) in
  match what_i_found with
  | hd :: _ -> hd
  | _ -> '0'

(* Solution for part 1 *)
let part2 (Input i : input) : answer =
  i
  |> List.map ~f:(fun {one; two} -> List.append one two)
  |> List.chunks_of ~length:3
  |> List.map ~f:find_item_in_many
  |> List.map ~f:item_priority
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer x)

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"
