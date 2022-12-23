open Base

exception Not_implemented

type direction = North | East | South | West
type x_y_coord = int * int 
type input = Input of x_y_coord list
type answer = Answer of int | Unknown


(* Answer print function *)
let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


  (* Parse input functions *)
let text_to_input (t: string) :input =
  t
  |> String.split_lines
  |> List.concat_mapi ~f:(
    fun y line -> List.filter_mapi (String.to_list line) ~f:(
      fun x c -> match c with | '#' -> Some (x, y) | _ -> None))
  |> fun x -> Input x


let area_boundaries (elves: x_y_coord list) : (int * int) * (int * int) =
  let min_el (els: int list) : int = List.min_elt els ~compare:(Int.compare) |> Option.value_exn in
  let max_el (els: int list) : int = List.max_elt els ~compare:(Int.compare) |> Option.value_exn in
  let min_x = elves |> List.map ~f:(fun (x, _) -> x) |> min_el in
  let max_x = elves |> List.map ~f:(fun (x, _) -> x) |> max_el in
  let min_y = elves |> List.map ~f:(fun (_, y) -> y) |> min_el in
  let max_y = elves |> List.map ~f:(fun (_, y) -> y) |> max_el in
  ((min_x, min_y), (max_x, max_y))


(* Debug functions *)
let print_elves_on_map (elves : x_y_coord list) : unit =
  let ((min_x, min_y), (max_x, max_y)) = area_boundaries elves in
  let exists (x, y) = 
    List.exists elves ~f:(fun (xe, ye) -> (xe-min_x)=x && (ye-min_y) = y) 
  in
  List.init (max_y - min_y + 1) ~f:(fun y -> List.init (max_x - min_x + 1) ~f:(fun x -> if exists (x, y) then '#' else ' '))
  |> List.map ~f:String.of_char_list
  |> List.iter ~f:(fun line -> Stdio.printf "|%s|\n" line)


(* Solution helper functions *)
let need_to_move ((x, y) : x_y_coord) (elves : x_y_coord list) : bool =
  (* function that checks whether another elf is close to elf *)
  let close_presence (xi, yi) = 
    (x - 1) <= xi && xi <= (x + 1) && 
    (y - 1) <= yi && yi <= (y + 1) &&
    x <> xi && y <> yi
  in
  let count_nbr_elves = List.count elves ~f:close_presence in
  count_nbr_elves > 0

let propose_move ((x, y) : x_y_coord) (elves : x_y_coord list) (options: direction list) : x_y_coord =
  let no_elves_in_direction direction = 
    let direction_coordinares = match direction with
    | North -> [(x-1, y-1); (x, y-1); (x+1, y-1)] 
    | East -> [(x+1, y-1); (x+1, y); (x+1, y+1)] 
    | South -> [(x-1, y+1); (x, y+1); (x+1, y+1)] 
    | West -> [(x-1, y-1); (x-1, y); (x-1, y+1)]
    in
    let elves_count = 
    List.count direction_coordinares ~f:(
      fun (xd, yd) -> List.exists elves ~f:(
        fun (xe, ye) -> xd = xe && yd = ye))
    in 
    elves_count = 0
  in
  let move_in_direction = function
  | North -> (x, y-1)
  | East -> (x+1, y)
  | South -> (x, y+1)
  | West -> (x-1, y)
  in
  match 
    options |> List.filter_map ~f:(
      fun d -> if no_elves_in_direction d then Some d else None)
  with
  | [] -> (x, y)
  | d::_ -> move_in_direction d

let round (elves: x_y_coord list) (options: direction list) : x_y_coord list =
  let proposals = List.map elves ~f:(fun e -> 
    if need_to_move e elves then propose_move e elves options else e)
  in 
  (* repeat it again and again because not all elves have moved *)
  List.fold elves ~init:proposals ~f:(fun proposals' _ ->
    List.map2_exn proposals' elves ~f:(
      fun (x1, y1) (x0, y0) -> 
        (* check how many elves are going to end up in that place *)
        let cnt = List.count proposals ~f:(fun (xp, yp) -> xp = x1 && yp = y1) in
        (* if more than 1 (because 1 is current one) then stay on the same place *)
        if cnt > 1 then (x0, y0) else (x1, y1)
    )
  ) 

let play_n_rounds (num: int) (elves: x_y_coord list) : x_y_coord list =
  let initial_options = [North ; South ; West ; East] in
  let rec iter (n: int) (elves: x_y_coord list) (options: direction list) : x_y_coord list =
    if n = 0 then 
      elves 
    else
      let new_elves = round elves options in
      let new_options = List.split_n options 3 |> fun (l1, l2) -> List.append l2 l1 in
      Stdio.printf "Round %d:\n" (num - n + 1);
      print_elves_on_map new_elves;
      iter (n - 1) new_elves new_options
  in
  iter num elves initial_options

let area (elves: x_y_coord list) : int =
  let ((min_x, min_y), (max_x, max_y)) = area_boundaries elves in
  (max_x - min_x + 1) * (max_y - min_y + 1)


(* Solution for part 1 *)
let part1 (Input elves : input) : answer = 
  let count_elves = List.length elves in
  let new_elves = play_n_rounds 10 elves in
  let area_size = area new_elves in
  Answer (area_size - count_elves)


(* Solution for part 2 *)
let part2 (Input _ : input) : answer = Unknown
