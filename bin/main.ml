module D01 = Aoc2022_ocaml.Day_01
module D02 = Aoc2022_ocaml.Day_02
module D03 = Aoc2022_ocaml.Day_03
module D04 = Aoc2022_ocaml.Day_04
module D05 = Aoc2022_ocaml.Day_05
module D06 = Aoc2022_ocaml.Day_06
module D07 = Aoc2022_ocaml.Day_07
module D08 = Aoc2022_ocaml.Day_08
module D09 = Aoc2022_ocaml.Day_09
module D10 = Aoc2022_ocaml.Day_10
module D11 = Aoc2022_ocaml.Day_11
module D12 = Aoc2022_ocaml.Day_12
module D13 = Aoc2022_ocaml.Day_13
module D14 = Aoc2022_ocaml.Day_14
module D15 = Aoc2022_ocaml.Day_15
module D16 = Aoc2022_ocaml.Day_16
module D17 = Aoc2022_ocaml.Day_17
module D18 = Aoc2022_ocaml.Day_18
module D19 = Aoc2022_ocaml.Day_19
module D20 = Aoc2022_ocaml.Day_20
module D21 = Aoc2022_ocaml.Day_21
module D22 = Aoc2022_ocaml.Day_22
module D23 = Aoc2022_ocaml.Day_23

module Today = D23

(* this main function reads sandard input with puzzle data from file and returns standard output with answer *)
let () =
  let open Stdio in
  (*let solver = Day_01 in*)
  let input_text = In_channel.input_all Stdio.stdin in
  let answer1_text = input_text
    |> Today.text_to_input
    |> Today.part1
    |> Today.answer_to_text
  in
  let answer2_text = input_text
    |> Today.text_to_input
    |> Today.part2
    |> Today.answer_to_text
  in
  printf
    "\nPart 1 solution is: %s;\nPart 2 solution is: %s.\n"
    answer1_text answer2_text

