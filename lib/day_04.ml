open Base

type assignment = {from_id: int; to_id: int}

type input = Input of (assignment * assignment) list
type answer = Answer of int | Unknown

(* take string of format "number number" and return assignment *)
let parse_one_assignment (s:string) : assignment =
  let parts = String.split s ~on:'-' in
  {
    from_id = Int.of_string @@ List.hd_exn parts ;
    to_id = Int.of_string @@ List.last_exn parts ;
  }
let list_of_2_to_tupple (l: 'a list) : 'a * 'a =
  (List.hd_exn l, List.last_exn l)

let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.map ~f:(String.split ~on:',')
  |> List.map ~f:(fun x -> List.map x ~f:parse_one_assignment)
  |> List.map ~f:list_of_2_to_tupple
  |> (fun x -> Input x)

(* check that this assignment fully covers that assignment *)
let fully_covers this that =
  this.from_id <= that.from_id && this.to_id >= that.to_id

(* check that this assignment overlaps with that assignment *)
let overlaps this that =
  this.from_id <= that.to_id && this.to_id >= that.from_id

(* Solution for part 1 *)
let part1 (Input i : input) : answer =
  i
  (* find pairs where one assignment is fully covering another *)
  |> List.filter ~f:(fun (a1, a2) -> fully_covers a1 a2 || fully_covers a2 a1)
  (* how many pairs found? *)
  |> List.length
  |> (fun x -> Answer x)

(* Solution for part 1 *)
let part2 (Input i : input) : answer =
  i
  (* find pairs where one assignment is fully covering another *)
  |> List.filter ~f:(fun (a1, a2) -> overlaps a1 a2)
  (* how many pairs found? *)
  |> List.length
  |> (fun x -> Answer x)

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"
