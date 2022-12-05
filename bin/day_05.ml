open Base

type proc = { from_: int ; to_: int ; num_: int }
type stack = char list list
type input = Input of { stacks: stack ; procs: proc list }

type answer = Answer of char list | Unknown
 
type t1 = char list array

let grow_one_stack (lines : string list) (position: int) : char list =
  lines
  |> List.map ~f:(fun x -> String.get x position)

let parse_stacks (lines : string list) : stack = 
  (* first let's find that line with numbers of stacks - it is last line before empty line *)
  let line_with_nums = List.hd_exn @@ List.drop (List.rev lines) 1 in
  (* second - get list of numbers in it*)
  let nums = line_with_nums
  |> String.split ~on:' '
  |> List.filter_map ~f:(function | "" -> None | x -> Some (Int.of_string x)) in
  let positions = List.init (List.length nums) ~f:(fun x -> 1 + x * 3) |> List.rev in
  
  (* reverse lines so that stacks grow down *)
  List.drop (List.rev lines) 2
  (* drop 2 lines : empty line and the line with stack numbers *)
  |> (fun lns -> List.map positions ~f:(fun pos -> grow_one_stack lns pos))

let parse_procs (_ : string list) = [{ from_ = 1 ; to_ = 2 ; num_ = 3 }]
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

(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown

(* Solution for part 1 *)
let part2 (Input _ : input) : answer = Unknown

let answer_to_text = function
  | Answer x -> String.of_char_list x
  | Unknown  -> "Solution not yet implemented"
