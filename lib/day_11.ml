open Base

type t = Empty
exception Not_implemented

type monkey = {
  id : int ;
  items : int list ;
  operation : (int -> int) ;
  test : (int -> bool) ;
  divisible_by : int ;
  if_true : int ;
  if_false : int ;
  inspected : int
}

type thrown_item = { to_monkey : int ; item : int}

type input = Input of monkey list
type answer = Answer of int | Unknown

(* Parse input functions *)

(* take chunk of lines form input file and parse it into monkey type *)
let parse_one_monkey (text:string list) : monkey =
  let parse_op (v1: string) (op: string) (v2: string) : (int -> int) =
    (fun x ->
      let f = match op with | "+" -> Int.(+) | "*" -> Int.( * ) | _ -> raise (Not_implemented) in
      let a = match v1 with | "old" -> x | _ -> Int.of_string v1 in
      let b = match v2 with | "old" -> x | _ -> Int.of_string v2 in
      f a b
    )
  in
  let id =
    List.nth_exn text 0
    |> String.chop_prefix_exn ~prefix:"Monkey "
    |> String.chop_suffix_exn ~suffix:":"
    |> Int.of_string
  in
  let items =
    List.nth_exn text 1
    |> String.chop_prefix_exn ~prefix:"  Starting items: "
    |> String.split_on_chars ~on:[',' ; ' ']
    |> List.filter_map ~f:(function | "" -> None | x -> Some (Int.of_string x))
  in
  let operation =
    List.nth_exn text 2
    |> String.chop_prefix_exn ~prefix:"  Operation: new = "
    |> String.split_on_chars ~on:[' ']
    |> (fun x -> List.(parse_op (nth_exn x 0) (nth_exn x 1) (nth_exn x 2)))
  in
  let divisible_by : int =
    List.nth_exn text 3
    |> String.chop_prefix_exn ~prefix:"  Test: divisible by "
    |> Int.of_string
  in
  let test : (int -> bool) =
    fun v -> v % divisible_by = 0
  in
  let if_true =
    List.nth_exn text 4
    |> String.chop_prefix_exn ~prefix:"    If true: throw to monkey "
    |> Int.of_string
  in
  let if_false =
    List.nth_exn text 5
    |> String.chop_prefix_exn ~prefix:"    If false: throw to monkey "
    |> Int.of_string
  in
  { id ; items ; operation ; divisible_by ; test ; if_true ; if_false ; inspected = 0 }


let text_to_input (t: string) :input =
  t
  |> String.split_lines
  (* break lines into subgroups divided by empty line
     these groups contain information about monkey *)
  |> List.group ~break:(fun a _ -> String.equal a "")
  |> List.map ~f:parse_one_monkey
  |> (fun x -> Input x)


(* Debug functions *)


let debug_turn (number: int) (round: monkey list) : unit =
  let open List in
  let printf = Stdio.printf in
  printf "\n---------- turn %d -----------\n" number;
  iter round ~f:(fun m ->
    let items = (m.items |> map ~f:Int.to_string |> String.concat ~sep:",") in
    printf "Monkey: %d; holds items: %s; inspected items count: %d\n" m.id items m.inspected)


let debug_round (number: int) (round: monkey list) : unit =
  let open List in
  let printf = Stdio.printf in
  printf "\n---------- ROUND %d ----------\n" number;
  iter round ~f:(fun m ->
    let items = (m.items |> map ~f:Int.to_string |> String.concat ~sep:",") in
    printf "Monkey: %d; holds items: %s; inspected items count: %d\n" m.id items m.inspected)


(* solution helper functions *)


(* Monkey inspects items
   scores are recalculated
   and it is decided where those items are thrown *)
let throw_items (m:monkey) (relief_factor:int) : (thrown_item list) =
  let thrown_items = m.items
  (* increase worry level *)
  |> List.map ~f:m.operation
  (* lose interest *)
  |> List.map ~f:(fun x -> x / relief_factor)
  (* chose direction in which items are thrown *)
  |> List.map ~f:(fun item -> match m.test item with | true -> { to_monkey = m.if_true ; item } | false -> { to_monkey = m.if_false ; item })
  in
  thrown_items


(* After monkey throws items they need to be redistributed
  and numbers need to be reduced to avoid overflows *)
let redistribute_items (current: monkey) (monkeys: monkey list) (items: thrown_item list) : monkey list =
  let open List in

  (* common multiplier that can be used to
     reduce number keeping their "divisible by" property.
     no need to fight for least common multiplier *)
  let divisor = fold monkeys ~init:1 ~f:(fun z x -> z * x.divisible_by) in

  (* update info about monkey:
     if this is current monkey then clean up list of kept items
     and update counter of inspected imens
     else - add trown items towards this monkey*)
  let process_one_monkey (m: monkey) : monkey =
    let items_to_receive = filter_map items ~f:(
      fun {to_monkey ; item} -> if to_monkey = m.id then Some item else None
    )
    in
    if m.id = current.id
      then {m with items = [] ; inspected = m.inspected + length m.items}
    else {m with items = append m.items items_to_receive }
  in
  monkeys
  |> map ~f:process_one_monkey
  |> map ~f:(fun m -> { m with items = map m.items ~f:(fun x -> x % divisor)})


(* walk through all monkeys and let them inspect and through items to each other *)
let play_round (monkeys : monkey list) (relief_factor : int) : (monkey list) =
  let rec play_turn (turns: int list) (acc_state : monkey list) : (monkey list) =
    match turns with
    | [] -> acc_state
    | turn::ts ->
      let m = List.nth_exn acc_state turn in
      let thrown_items = throw_items m relief_factor in
      let new_acc = redistribute_items m acc_state thrown_items in
      (*debug_turn (turn + 1) new_acc;*)
      play_turn ts new_acc
  in
  play_turn (List.init (List.length monkeys) ~f:(fun x -> x)) monkeys


(* Solution for part 1 *)
let solve_part1 (Input i : input) : answer =
  List.init 20 ~f:(fun x -> x + 1)
  |> List.fold ~init:i ~f:(fun z n -> let r = play_round z 3 in debug_round n r; r)
  |> List.map ~f:(fun m -> m.inspected)
  |> List.sort ~compare:Int.compare
  |> (fun x -> List.take (List.rev x) 2)
  |> List.fold ~init:1 ~f:( * )
  |> (fun x -> Answer x)


(* Solution for part 2 *)
let solve_part2 (Input i : input) : answer =
List.init 10000 ~f:(fun x -> x + 1)
|> List.fold ~init:i ~f:(
  fun z n ->
  let r = play_round z 1 in
  (if n % 1000 = 0 then debug_round n r else ()); r)
|> List.map ~f:(fun m -> m.inspected)
|> List.sort ~compare:Int.compare
|> (fun x -> List.take (List.rev x) 2)
|> List.fold ~init:1 ~f:( * )
|> (fun x -> Answer x)


let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
