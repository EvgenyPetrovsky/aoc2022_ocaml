module D01 = Day_01
module D02 = Day_02
module D03 = Day_03
module D04 = Day_04
module D05 = Day_05
module D06 = Day_06
module D07 = Day_07
module D08 = Day_08
module D09 = Day_09
module D10 = Day_10

module Today = Day_10

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
    "Part 1 solution is: %s;\nPart 2 solution is: %s.\n"
    answer1_text answer2_text

