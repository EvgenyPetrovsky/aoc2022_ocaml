open Base

type shape = Rock | Paper | Scissors
type outcome = Win | Loss | Draw

type strategy1 = {opp: shape; you: shape}
type strategy2 = {opp: shape; res: outcome}

type input = Input of (strategy1 list * strategy2 list)

type answer = Answer of int | Unknown

let parse_one_line1 (str:string) : strategy1 =
  let (o, y) = (String.get str 0, String.get str 2) in
  let parse c = match c with
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    (* Yes, I know it is stupid to hide warning that way *)
    | _ -> Rock
  in
  {
    opp = parse o;
    you = parse y
  }

let parse_one_line2 (str:string) : strategy2 =
  let (o, y) = (String.get str 0, String.get str 2) in
  let parse_o c = match c with
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    (* Yes, I know it is stupid to hide warning that way *)
    | _ -> Rock
  in
  let parse_r c =
    match c with | 'X' -> Loss | 'Y' -> Draw | 'Z' -> Win | _ -> Loss
  in
  {
    opp = parse_o o;
    res = parse_r y
  }

let text_to_input (t : string) : input = t
  |> String.split_lines
  |> List.filter ~f:(String.(<>) "")
  |> List.map ~f:(fun x -> (parse_one_line1 x, parse_one_line2 x))
  |> List.unzip
  |> (fun x -> Input x)

let shape_score = function | Rock -> 1 | Paper -> 2 | Scissors -> 3

let outcome_score (x : outcome) : int =
match x with | Loss -> 0 | Draw -> 3 | Win -> 6

let round_outcome (opp:shape) (you:shape) : outcome =
  match (opp, you) with
  | (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) -> Win
  | (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) -> Draw
  | _ -> Loss

let round_score (opp: shape) (you: shape) : int =
  (round_outcome opp you |> outcome_score) + (shape_score you)

let your_shape (opp:shape) (res: outcome) : shape =
  match (opp, res) with
    | (Rock, Loss) | (Paper, Win) | (Scissors, Draw) -> Scissors
    | (Paper, Loss) | (Scissors, Win) | (Rock, Draw) -> Rock
    | _ -> Paper

let solve_part1 (Input (i, _)) : answer = i
  |> List.map ~f:(fun ({opp; you}:strategy1) -> round_score opp you)
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer x)

let solve_part2 (Input (_, i)) : answer = i
  |> List.map ~f:(fun ({opp; res}:strategy2) -> round_score opp (your_shape opp res))
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer x)

let answer_to_text = function
| Answer x -> Int.to_string x
| Unknown  -> "Solution not yet implemented"


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
