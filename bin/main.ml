open Stdio

(* this main function reads sandard input with puzzle data from file and returns standard output with answer *)
let () =
  (*let solver = Day_01 in*)
  let input_text = In_channel.input_all Stdio.stdin in
  let answer1_text = input_text
    |> Day_04.text_to_input
    |> Day_04.part1
    |> Day_04.answer_to_text
  in
  let answer2_text = input_text
    |> Day_04.text_to_input
    |> Day_04.part2
    |> Day_04.answer_to_text
  in
  printf
    "Part 1 solution is: %s;\nPart 2 solution is: %s.\n"
    answer1_text answer2_text

