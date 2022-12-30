module type Solvable = sig
  type input
  type answer
  val text_to_input : string -> input
  val solve_part1 : input -> answer
  val solve_part2 : input -> answer
  val show_answer : answer -> string
end

module type Solution = sig
  val part1 : string -> string
  val part2 : string -> string
end

module Make (Day: Solvable) :
  Solution =
struct
  let part1 = fun (x: string) : string -> x |> Day.text_to_input |> Day.solve_part1 |> Day.show_answer
  let part2 = fun (x: string) : string -> x |> Day.text_to_input |> Day.solve_part2 |> Day.show_answer
end
