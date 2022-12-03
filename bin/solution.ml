open Base

module type SOLUTION = sig
  type text = string
  type input
  val text_to_input : text -> input
  type answer
  type solution = input -> answer
  type part1 = solution
  type part2 = solution
  val answer_to_text : answer -> string
end
