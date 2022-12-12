open Base

type proc = { from_: int ; to_: int ; num_: int }
type stack = char list
type crane_model = CM9000 | CM9001
type input = Input of { stacks: stack list ; procs: proc list }

type answer = Answer of char list | Unknown

let read_crate_by_stack_num (text_line:string) (stack_num:int) : char =
  (* position of first stack is in character with index 1 (starts with 0) *)
  let pos = 4 * stack_num - 3 in
  if pos > String.length text_line then ' ' else String.get text_line pos

(* stacks are defined as few lines in the beginning of file
   in the bottom of stacks are the numbers 1 for first, 2 for second, etc
   stacks are going to be represented as lists *)
let parse_stacks (lines : string list) : (stack list)=
  (* first let's find that line with numbers of stacks - it is last line before empty line *)
  let line_with_nums = List.hd_exn @@ List.drop (List.rev lines) 1 in
  (* ... and  lines that contain crates *)
  let lines_with_crates = List.rev @@ List.drop (List.rev lines) 2 in
  (* second - get list of numbers in it*)
  let stack_nums =
    line_with_nums
    |> String.split ~on:' '
    |> List.filter_map ~f:(function | "" -> None | x -> Some (Int.of_string x))
  in
  stack_nums
  |> List.map ~f:(fun s -> List.map lines_with_crates ~f:(fun l -> read_crate_by_stack_num l s))
  |> List.map ~f:(fun s -> List.filter s ~f:(fun c -> Char.(<>) c ' '))


(* procedures are lines of format "move N from N to N" *)
let parse_procs (procs : string list) =
  let ios = Int.of_string in
  let parse_line (line: string) : proc option =
    match ( String.split ~on:' ' line ) with
    | "move"::n::"from"::s::"to"::t::_ -> Some { from_ = ios s ; to_ = ios t ; num_ = ios n }
    | _ -> None
  in
  procs
  |> List.filter_map ~f:parse_line

let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.group ~break:(fun x _ -> String.(x = ""))
  |> (fun lst ->
       let stk = List.hd_exn lst in
       let prc = List.hd_exn @@ List.drop lst 1 in
       let stacks = parse_stacks stk in
       let procs  = parse_procs prc in
       Input {stacks ; procs})

let rec restack_by_1 (from_ : stack) (to_ : stack ) (n : int) : (stack * stack) =
  match (from_, to_, n) with
  | ([], _, _) | (_, _, 0) -> (from_, to_)
  | ((hd::tl), _, n) -> restack_by_1 tl (hd::to_) (n-1)

let restack_by_n (from_ : stack) (to_ : stack ) (n : int) : (stack * stack) =
  let (move, keep) = List.split_n from_ n in
  (keep, List.append move to_)

let move_crates (stacks: stack list) (proc: proc) (model: crane_model) : (stack list) =
  let a = Array.of_list stacks in
  let (from_, to_, n) = (proc.from_ - 1, proc.to_ - 1, proc.num_) in
  let (s1, s2) = match model with
  | CM9000 -> restack_by_1 (Array.get a from_) (Array.get a to_) n
  | CM9001 -> restack_by_n (Array.get a from_) (Array.get a to_) n
  in
  Array.set a from_ s1; Array.set a to_ s2;
  Array.to_list a

(* Solution for part 1 *)
let part1 (Input { stacks ; procs } : input) : answer =
  procs
  |> List.fold ~init:stacks ~f:(fun z x -> move_crates z x CM9000)
  |> List.filter_map ~f:List.hd
  |> (fun x -> Answer x)

(* Solution for part 1 *)
let part2 (Input { stacks ; procs } : input) : answer =
procs
|> List.fold ~init:stacks ~f:(fun z x -> move_crates z x CM9001)
|> List.filter_map ~f:List.hd
|> (fun x -> Answer x)

let answer_to_text = function
  | Answer x -> String.of_char_list x
  | Unknown  -> "Solution not yet implemented"
