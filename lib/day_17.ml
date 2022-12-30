open Base

type t = Empty
exception Not_implemented
exception Invalid_case

type chamber_space_unit = | Rock | Air | Wall
type chamber_level = chamber_space_unit array
type chamber = chamber_level array

type direction = | Left | Right | Down

type input = Input of direction list
type answer = Answer of int | Unknown
type rock_coords = (int * int) list

let starting_offset_x : int = 2
let starting_offset_y : int = -1

let chamber_width : int = 7
let rock_shapes =
  [
    [ "####" ] ;
    [
      " # " ;
      "###" ;
      " # "
    ] ;
    [
      "  #" ;
      "  #" ;
      "###"
    ] ;
    [
      "#" ;
      "#" ;
      "#" ;
      "#"
    ] ;
    [
      "##" ;
      "##"
    ]
  ]

let parse_rock_shape_coords (shapes: string list list ) : (rock_coords list) =
  let parse_shape (shape : string list) : (rock_coords) =
    List.concat_mapi shape ~f:(
      (* first we move by lines - this is Y coordinate*)
      fun y s ->
        String.to_list s
        |> List.filter_mapi ~f:(
          (* then we move by characters - this is X coordinate *)
          fun x c -> match c with
          | '#' -> Some (x + starting_offset_x, y + starting_offset_y)
          | _ -> None)
    )
  in
  List.map shapes ~f:parse_shape

(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* Parse input functions *)
let text_to_input (t: string) : input =
  let parse_char =
    function | '<' -> Left | '>' -> Right | _ -> raise Invalid_case
  in
  t
  |> String.split_lines
  |> List.concat_map ~f:String.to_list
  |> List.map ~f:parse_char
  |> (fun x -> Input x)


(* Debug functions *)
let debug_rock (rock:rock_coords) : unit =
  rock
  |> List.map ~f:(fun (x, y) -> "(" ^ (Int.to_string x) ^ "," ^ (Int.to_string y) ^ ")")
  |> String.concat ~sep:"-"
  |> (fun x -> Stdio.printf "\nRock: %s\n" x)

let debug_chamber (chamber: chamber) : unit =
  let strs =
  chamber
  |> Array.map ~f:(
    fun units ->
      Array.map units ~f:(function | Air -> '.' | _ -> '#') |> Array.to_list |> String.of_char_list
  )
  in
  let strs = Array.append strs [|"-------"|] in
  Stdio.printf "\nChamber:\n%s\n" (String.concat ~sep:"\n" @@ Array.to_list strs)

let debug_move (dir:direction) : unit =
  let dir_chr = match dir with | Left -> '<' | Right -> '>' | Down -> 'v' in
  Stdio.printf "%c" dir_chr

(* Solution helper functions *)
let provision_empty_levels ~(number:int) (chamber:chamber) : chamber =
  let trimmed_chamber = Array.filter chamber ~f:(fun x -> Array.exists x ~f:(function | Rock -> true | _ -> false)) in
  let new_levels = Array.init number ~f:(fun _ -> Array.init chamber_width ~f:(fun _ -> Air)) in
  Array.append new_levels trimmed_chamber

(* move one level down *)
let valid_rock_position (rock: rock_coords) (chamber: chamber) : bool =
  let chamber_depth = Array.length chamber in
  List.for_all rock ~f:(
    fun (x, y) ->
      if x < 0 || x >= chamber_width || y >= chamber_depth then false
      else match chamber.(y).(x) with | Air -> true | _ -> false)

let move_rock (rock: rock_coords) (direction: direction) : rock_coords =
  let (dx, dy) = match direction with
  | Left -> (-1, 0)
  | Right -> (1, 0)
  | Down -> (0, 1)
  in
  List.map rock ~f:(fun (x, y) -> (x + dx, y + dy))

let fix_rock_in_chamber (rock: rock_coords) (chamber: chamber) : chamber =
  (*debug_rock rock;*)
  List.iter rock ~f:(fun (x, y) -> chamber.(y).(x) <- Rock);
  (*debug_chamber chamber;*)
  chamber

let rock_heigth (rock:rock_coords): int =
  (* add 2 because initial y position = 0 is still height of 1 and initial y offset is -1 *)
  List.fold rock ~init:0 ~f:(fun z (_,y) -> max z (y + 1 - starting_offset_y ))

let play ~(num: int) (rock_sequence: rock_coords list) (jet_sequence: direction list) (chamber: chamber) : chamber =
  let rec iter r_num r r_seq j_seq chamber =
    let r_seq = match r_seq with | [] -> rock_sequence | _ -> r_seq in
    let j_seq = match j_seq with | [] -> jet_sequence | _ -> j_seq in
    if r_num = 0 then chamber
    else
      let new_dn_pos = move_rock r Down in
      if valid_rock_position new_dn_pos chamber then
        let j = List.hd_exn j_seq in
        let new_j_seq = List.tl_exn j_seq in
        let new_jt_pos = move_rock new_dn_pos j in
        let new_rock_pos = if valid_rock_position new_jt_pos chamber then new_jt_pos else new_dn_pos in
        (*debug_move j;*)
        iter r_num new_rock_pos r_seq new_j_seq chamber
      else
        let next_r = List.hd_exn r_seq in
        let r_h = rock_heigth next_r in
        let new_chamber = fix_rock_in_chamber r chamber |> provision_empty_levels ~number:(r_h + 3) in
        iter (r_num-1) next_r (List.tl_exn r_seq) j_seq new_chamber
  in
  let r = List.hd_exn rock_sequence in
  let r_h = rock_heigth r in
  iter num r (List.tl_exn rock_sequence) (jet_sequence) (provision_empty_levels ~number:(r_h + 3) chamber)
(* Solution for part 1 *)
let solve_part1 (Input jet_sequence : input) : answer =
  let n = 2022 in
  let rock_sequence = parse_rock_shape_coords rock_shapes in
  let chamber : chamber = [||] in
  let new_chamber = play ~num:n rock_sequence jet_sequence chamber in
  new_chamber
  |> Array.count ~f:(fun a -> Array.exists a ~f:(function | Rock -> true | _ -> false))
  |> (fun x -> Answer x)


(* Solution for part 2 *)
(* approach: take relatively big number of iterations, 
   do them by fixing the heigth of stack after every iteration
   translate heigths into deltas 
   use Floyd's tortoise and hare algorigthm to detect cycle 
   the resulting heigth of stack will be heighs at start of cycles + number of 
   cycles up to number n = 1_000_000_000_000 *)
let solve_part2 (Input jet_sequence : input) : answer =
  (* trying to sneak in hope there is some stable pattern that repeats *)
  let n = 1_000_000_000_000 in
  let rock_sequence = parse_rock_shape_coords rock_shapes in
  let period = (List.length jet_sequence) * (List.length rock_sequence) * 1 in
  let n_div = (n / period) - 4 in
  let n_rem = (n % period) + (4 * period) in
  (* define initial offset for the cave: 
     reminder division of n by period 
     plus period, in case reminder is zero *)
  let tower_h_0 =
    play ~num:(n_rem + 0 * period) rock_sequence jet_sequence [||] |>
    Array.count ~f:(fun a -> Array.exists a ~f:(function | Rock -> true | _ -> false))
  in
  (* measure the heigth of tower after initial offset + 1 full period
     in hope that it will give us delta growth for every period *)
  let tower_h_1 =
    play ~num:(n_rem + 1 * period) rock_sequence jet_sequence [||] |>
    Array.count ~f:(fun a -> Array.exists a ~f:(function | Rock -> true | _ -> false))
  in
  Answer (tower_h_0 + (n_div - 1) * (tower_h_1 - tower_h_0))


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
