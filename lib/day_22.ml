open Base

exception Not_implemented
exception Invalid_case

type tile = Void | Open | Closed
type action = Left | Right | Forward of int
type map = tile array array
type input = Input of { map: map ; actions: action list }
type direction = North | East | South | West
type answer = Answer of int | Unknown
type position = {
  location: int * int ;
  direction: direction ;
  path: (int * int) list
  }

(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* Parse input functions *)

(* Take file, split contents by empty line
   first part of content is a map that goes into map type with values of Void, Open, Closed
   second part is instructions that can be Forward <number of steps>, Left, Right *)
let text_to_input (t: string) :input =
  let (map_part, act_part) = t
  |> String.split_lines
  |> List.split_while ~f:String.(fun x -> x <> "")
  in
  let map = map_part
  |> List.to_array
  |> Array.map ~f:String.to_array
  |> Array.map ~f:(Array.map ~f:(function | '.' -> Open | '#' -> Closed | _ -> Void))
  in
  (* parse actions string character by character
     when character is R -> Right, if L -> Left,
     otherwise it must be a digit and we read the characters
     until non-digit one and conver them into number of Forward steps *)
  let rec parse_actions (acts: char list) : action list =
    match acts with
    | [] -> []
    | 'R'::following -> Right :: (parse_actions following)
    | 'L'::following -> Left :: (parse_actions following)
    | x ->
      let (ns, following) = List.split_while x ~f:Char.(fun c -> c <> 'L' && c <> 'R') in
      Forward (Int.of_string (String.of_char_list ns)) :: parse_actions following
  in
  let actions =
    act_part
    |> List.filter ~f:String.(fun x -> x <> "")
    |> List.hd_exn
    |> String.to_list
    |> parse_actions
  in
  Input { map ; actions }



(* Debug functions *)


(* Solution helper functions *)

(* initial position *)
let initialise_position (map: map) : position =
  let suitable_locations = Array.concat_mapi map ~f:(fun y row -> Array.filter_mapi row ~f:(fun x tile -> match tile with | Open -> Some (x, y) | _ -> None)) in
  let location = suitable_locations.(0) in
  let direction = East in
  let path = [] in
  { location ; direction ; path }

(* given direction return dx, dy *)
let direction_to_increment (dir:direction) : int * int =
  let (dx, dy) = match dir with
  | North -> ( 0,-1)
  | East  -> ( 1, 0)
  | South -> ( 0, 1)
  | West  -> (-1, 0)
  in
  (dx, dy)

let tile_of_coordinate map ((x, y) : int * int) : tile =
  map.(y).(x)

(* find next tile to which the move should lead
   fast forward over Void (non defined map field)
   land on Open tile
   or rollback to initial location if next tile is Closed *)
let next_location map (initial_location : int * int) (direction: direction) : (int * int) =
  let (dx, dy) = direction_to_increment direction in
  let (x_size, y_size) = (Array.length map.(0), Array.length map) in
  let rec iter (x, y) : int * int =
    (* make sure next coordinate doesn't go out of map bounds *)
    let next_coordinate = ((x + dx) % x_size, (y + dy) % y_size) in
    match tile_of_coordinate map next_coordinate with
    | Void -> iter next_coordinate
    | Closed -> initial_location
    | Open -> next_coordinate
  in
  iter initial_location

(* process action according to its type *)
let process_action (map : map) ({ location ; direction ; _ } as position : position) (action: action) : position =
  let new_position =
    match action with
    | Left  ->
      let new_dir = match direction with | North -> West | West -> South | South -> East | East -> North in
      { position with direction = new_dir }
    | Right ->
      let new_dir = match direction with | West -> North | South -> West | East -> South | North -> East in
      { position with direction = new_dir }
    | Forward steps ->
      let steps = List.init steps ~f:(fun x -> x + 1) in
      let new_loc = steps |> List.fold ~init:location ~f:(fun location _ -> next_location map location direction) in
      { position with location = new_loc }
  in
  let { direction ; location = (x, y) ; _ } = new_position in
  Stdio.printf "- location: Row = %d ; Column = %d ; Direction = %s\n" (y + 1) (x + 1) (match direction with | East -> "East" | South -> "South" | West -> "West" | North -> "North") ;
  new_position

let position_to_score ({ direction ; location = (x, y) ; _ }: position) : int =
  Stdio.printf "\nFinal location: Row = %d ; Column = %d ; Direction = %s\n" (y + 1) (x + 1) (match direction with | East -> "East" | South -> "South" | West -> "West" | North -> "North") ;
  (y + 1) * 1000 + (x + 1) * 4 + (match direction with | East -> 0 | South -> 1 | West -> 2 | North -> 3 )

(* Solution for part 1 *)
let solve_part1 (Input { map ; actions } : input) : answer =
  let initial_position = initialise_position map in
  let act = fun position action -> process_action map position action in
  actions
  |> List.fold ~init:initial_position ~f:act
  |> position_to_score
  |> fun x -> Answer x


(* Solution for part 2 *)
let solve_part2 (Input _ : input) : answer = Unknown


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
