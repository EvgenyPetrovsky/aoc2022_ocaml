
let usage_msg = "aoc2022_ocaml -day <day_number> [-input <puzzle_input_file>]"
let day = ref (-1)
let input_file = ref ""
let anon_fun filename = input_file := filename
let speclist =
  [
    ("-day", Arg.Set_int day, "Day number to solve");
    ("-file", Arg.Set_string input_file, "Puzzle input");
  ]

exception Invalid_day of string

let read_whole_input_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* this main function reads sandard input with puzzle data from file and returns standard output with answer *)
let () =
  let () = Arg.parse speclist anon_fun usage_msg in
  let (part1, part2) = match !day with
  |  1 -> Aoc2022_ocaml.Day_01.(part1, part2)
  |  2 -> Aoc2022_ocaml.Day_02.(part1, part2)
  |  3 -> Aoc2022_ocaml.Day_03.(part1, part2)
  |  4 -> Aoc2022_ocaml.Day_04.(part1, part2)
  |  5 -> Aoc2022_ocaml.Day_05.(part1, part2)
  |  6 -> Aoc2022_ocaml.Day_06.(part1, part2)
  |  7 -> Aoc2022_ocaml.Day_07.(part1, part2)
  |  8 -> Aoc2022_ocaml.Day_08.(part1, part2)
  |  9 -> Aoc2022_ocaml.Day_09.(part1, part2)
  | 10 -> Aoc2022_ocaml.Day_10.(part1, part2)
  | 11 -> Aoc2022_ocaml.Day_11.(part1, part2)
  | 12 -> Aoc2022_ocaml.Day_12.(part1, part2)
  | 13 -> Aoc2022_ocaml.Day_13.(part1, part2)
  | 14 -> Aoc2022_ocaml.Day_14.(part1, part2)
  | 15 -> Aoc2022_ocaml.Day_15.(part1, part2)
  | 16 -> Aoc2022_ocaml.Day_16.(part1, part2)
  | 17 -> Aoc2022_ocaml.Day_17.(part1, part2)
  | 18 -> Aoc2022_ocaml.Day_18.(part1, part2)
  | 19 -> Aoc2022_ocaml.Day_19.(part1, part2)
  | 20 -> Aoc2022_ocaml.Day_20.(part1, part2)
  | 21 -> Aoc2022_ocaml.Day_21.(part1, part2)
  | 22 -> Aoc2022_ocaml.Day_22.(part1, part2)
  | 23 -> Aoc2022_ocaml.Day_23.(part1, part2)
  | 25 -> Aoc2022_ocaml.Day_25.(part1, part2)
  |  _ -> raise (Invalid_day "AoC day is not specified or is invalid, please make sure the -day parameter is provided and is between 1 and 25")
  in
  let open Stdio in
  (*let solver = Day_01 in*)
  let input_text = if String.equal !input_file ""
    then In_channel.input_all Stdio.stdin
    else read_whole_input_file !input_file
  in
  let answer1_text = input_text |> part1 in
  let answer2_text = input_text |> part2 in
  printf
    "\nPart 1 solution is: %s;\nPart 2 solution is: %s.\n"
    answer1_text answer2_text

