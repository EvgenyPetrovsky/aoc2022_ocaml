open Base

type t = Empty
exception Not_implemented

type grid = int array
type forest = {size: int ; grid: grid}
type input = Input of forest
type direction = North | East | South | West
type projection = int array
type answer = Answer of int | Unknown

let array_to_string a =
  let array_string = a
  |> Array.map ~f:Int.to_string
  |> Array.to_list
  |> String.concat ~sep:"," in
  "[|"^array_string^"|]"
let text_to_input (t: string) :input =
  let lines = String.split_lines t in
  let size = List.length lines in
  let chars = String.concat lines |> String.to_array in
  let grid = Array.map chars ~f:(fun c -> Int.of_string @@ Char.to_string c) in
  (*Stdio.printf "%s\n" (array_to_string grid);*)
  Input {size ; grid}

(* function returns projection lines from certain point in grid to outside
  in 4 directions: North, East, South, West *)
let get_projections (idx: int) ({size ; _} : forest) : (projection array) =
  let idx_to_coord (idx: int) : (int * int) = (idx / size, idx % size) in
  let coord_to_idx ((row, col) : int * int) : int = row * size + col in
  let (row, col) = idx_to_coord idx in
  let full = Array.init (size * size) ~f:idx_to_coord in
  (* get projections and sort them from tree to outside *)
  let projections_coord =
    [|
      Array.filter full ~f:(fun (r, c) -> r = row && c > col);
      Array.filter full ~f:(fun (r, c) -> r = row && c < col) |> Array.rev;
      Array.filter full ~f:(fun (r, c) -> r < row && c = col) |> Array.rev;
      Array.filter full ~f:(fun (r, c) -> r > row && c = col)
    |]
  in
  let projection_idx = Array.map projections_coord ~f:(fun x -> Array.map x ~f:coord_to_idx) in
  (*Array.iter projection_idx ~f:(fun x -> Stdio.printf "%s\n" (array_to_string x) );*)
  projection_idx

(* check visibility of certain element of grid *)
let is_visible ~(tree_idx: int) ~(in_forest: forest) : bool =
  let grid = in_forest.grid in
  let projections = get_projections tree_idx in_forest in
  let idx_h = Array.get grid tree_idx in
  let prj_h = Array.map projections ~f:(fun prj -> Array.map prj ~f:(fun idx -> Array.get grid idx)) in
  let prj_v = Array.map prj_h ~f:(
    function
    (* if tree is not behind others - it is visible *)
    | [||] -> true
    (* otherwise it must be higher than trees in front of it *)
    | prj -> Array.for_all prj ~f:(fun h -> h < idx_h)
  ) in

  Array.exists prj_v ~f:(fun x -> x)

let scenic_score ~(tree_idx:int) ~(in_forest: forest) : int =
  let grid = in_forest.grid in
  let projections = get_projections tree_idx in_forest in
  let idx_h = Array.get grid tree_idx in
  let prj_h = Array.map projections ~f:(fun prj -> Array.map prj ~f:(fun idx -> Array.get grid idx)) in
  let rec count_fun = function
    | [] -> 0
    | l::ls -> if idx_h <= l then 1 else 1 + count_fun ls
  in
  let cnt_t = Array.map prj_h ~f:(fun x -> count_fun @@ Array.to_list x) in
  Array.fold cnt_t ~init:1 ~f:( * )

(* Solution for part 1 *)
let solve_part1 (Input forest : input) : answer =
  let id x = x in
  forest.grid
  |> Array.mapi ~f:(fun idx _ -> is_visible ~tree_idx:idx ~in_forest:forest)
  |> Array.filter ~f:id
  |> Array.length
  |> (fun x -> Answer x)

(* Solution for part 1 *)
let solve_part2 (Input forest : input) : answer =
  forest.grid
  |> Array.mapi ~f:(fun idx _ -> scenic_score ~tree_idx:idx ~in_forest:forest)
  |> Array.fold ~init:0 ~f:max
  |> (fun x -> Answer x)

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
