open Base

type input = Input of int list list
type answer = Answer of int | Unknown

(* function that extracts integer number from the string *)
let extract_int = function
  | "" -> None
  | t  -> Some (Int.of_string t)

let text_to_input (t: string) :input = t
  (* Split raw text into lines first*)
  |> String.split_lines
  (* Some lines are empty and we use it as a separator to create list of lists*)
  |> List.group ~break:(fun x _ -> String.(x = ""))
  (* filter out empty element from nested lists and convert elements to int *)
  |> List.map ~f:(fun x -> List.filter_map x ~f:extract_int)
  |> (fun x -> Input x)

(* Solution for part 1 *)
let part1 (Input i : input) : answer = i
  (* Sum elements of nested lists *)
  |> List.map ~f:(List.fold ~init:0 ~f:((+)))
  (* select biggest number *)
  |> List.max_elt ~compare:(Int.compare)
  (* biggest number is returned as optional type, use 0 as default when None *)
  |> (fun x -> match x with | None -> Answer 0 | Some n -> Answer n)

(* Solution for part 2 *)
let part2 (Input i : input) : answer = i
  (* Sum elements of nested lists *)
  |> List.map ~f:(List.fold ~init:0 ~f:(+))
  (* sort list descending *)
  |> List.sort ~compare:(Int.compare)
  |> List.rev
  (* take biggest 3 elements - they go first after sorting *)
  |> (fun x -> List.take x 3)
  (* sum elements *)
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer x)

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"
