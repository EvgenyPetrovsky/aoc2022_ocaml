open Base

module type SOLUTION =
  sig
    type text = string list
    type input
    val text_to_input : text -> input
    type answer
    type solution = input -> answer
    type part1 = solution
    type part2 = solution
    val solution_to_text : answer -> string
  end

module Day_01  = struct
  type input = int list
  type answer = int

  let text_test =
    [ "1000"; "2000"; "3000"; ""; "4000"; ""; "5000"; "6000"; ""; "7000"; "8000"; "9000"; ""; "10000"]


end