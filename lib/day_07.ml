open Base

type t = Empty
type fs_item =
  | Directory of { name: string ; size: int option ; content: fs_item list }
  | File of { name: string ; size: int }
type input = Input of fs_item
type answer = Answer of int | Unknown
type line_type = Cmd_cd  of string | Cmd_ls | Out_d of string | Out_f of int * string

exception Item_not_found of string
exception Item_of_wrong_type of string

let line_type (line:string) : line_type =
  match String.split line ~on:' ' with
    | "$"::"cd"::dir::_ -> Cmd_cd dir
    | "$"::"ls"::_ -> Cmd_ls
    | "dir"::name::_  -> Out_d name
    | size::name::_   -> Out_f (Int.of_string size, name)
    | _ -> raise (Item_not_found line)

(* cd command changes current path: .. - removes directory, / goes to root, name - goes deeper into directory *)
let cd (path:string list) (dir:string) : (string list) =
  match dir with
  | "/" -> []
  | "." -> path
  | ".." -> path |> List.rev |> (fun x -> List.drop x 1) |> List.rev
  | _ -> path |> List.rev |> List.cons dir |> List.rev

(* add new file system item into overall file system tree *)
let rec add_fs_item (tree: fs_item) (item: fs_item) (path: string list) : fs_item =
  match tree with
  | File {name = _ ; size = _} -> tree
  | Directory {name; size; content} ->
    match path with
    | [] -> Directory { name ; size ; content = item::content }
    | p::ps ->
        let fltr = function
        | Directory {name ; _} -> String.(name = p)
        | _ -> false in
        let subdir = List.find content ~f:fltr in
        let others = List.filter content ~f:(fun x -> not @@ fltr x) in
        match subdir with
        | Some x -> Directory { name ; size = None ; content = (add_fs_item x item ps) :: others }
        | _ -> raise (Item_not_found p)

let parse_one_more_line (acc: string list * fs_item) (line: string) =
  let (path, tree) = acc in
  match line_type(line) with
  | Cmd_ls -> acc
  | Cmd_cd dir -> (cd path dir, tree)
  | Out_d dir ->
      let new_item = Directory {name = dir ; size = None ; content = []} in
      (path, add_fs_item tree new_item path)
  | Out_f (size, file) ->
      let new_item = File {name = file ; size = size} in
      (path, add_fs_item tree new_item path)

(* parse text data into file system structure of type fs_item *)
let text_to_input (t: string) :input =
  let start_path = ([]: string list) in
  let start_item = Directory {name = "" ; size = None ; content = []} in
  let init = (start_path, start_item) in
  t
  |> String.split_lines
  |> List.fold ~init:init ~f:parse_one_more_line
  |> (fun (_, tree) -> Input tree)

(* calculate one dir size based on nested file and dir sizes *)
let rec calc_one_dir_size (tree: fs_item) : int =
  match tree with
  | File { name = _ ; size } -> size
  | Directory { name = _ ; size = Some x ; _} -> x
  | Directory { name = _ ; size = None ; content} ->
    List.map content ~f:calc_one_dir_size |> List.fold ~init:0 ~f:(+)

(* directory sizes are optional for the initial creation of input
   but for solving the problem we need to calculate size of directories
   this function goes trhough all structure recursively and derives sizes *)
let rec calculate_dir_sizes (tree: fs_item) : fs_item =
  match tree with
  | File {name = _; size = _} as file -> file
  | Directory { name = _ ; size = Some _ ; _} as dir -> dir
  | Directory { name ; size = None ; content} as dir ->
    let dir_size = calc_one_dir_size dir in
    let new_content : fs_item list = List.map content ~f:(
      function
      | Directory _ as dir -> calculate_dir_sizes dir
      | x -> x)
    in Directory {name ; size = Some dir_size ; content = new_content}

(* get list if directory names and sizes from file system structure *)
let rec get_list_of_dirs (tree:fs_item list) : ((string * int) list) =
  match tree with
  | [] -> []
  | hd::tl -> match hd with
    | Directory {name ; size = Some dir_size ; content = nested_content} ->
      List.append [(name, dir_size)] @@ List.append (get_list_of_dirs nested_content) (get_list_of_dirs tl)
    | _ -> []

(* Solution for part 1 *)
let solve_part1 (Input i : input) : answer =
  let dirs = get_list_of_dirs [calculate_dir_sizes i] in
  dirs
  |> List.filter_map ~f:(fun(_, size) -> if size > 100_000 then None else Some size)
  |> List.fold ~init:0 ~f:(+)
  |> (fun x -> Answer x)

(* Solution for part 1 *)
let solve_part2 (Input i : input) : answer =
  let total_space = 70_000_000 in
  let used_space = calc_one_dir_size i in
  let required_space = 30_000_000 in
  let need_space = required_space - (total_space - used_space) in
  let dirs = get_list_of_dirs [calculate_dir_sizes i] in
  dirs
  |> List.filter_map ~f:(fun(_, size) -> if size < need_space then None else Some size)
  |> List.min_elt ~compare:Int.compare
  |> (function | Some x -> Answer x | None -> Unknown)

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"


(* end-to-end functions *)

let part1 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part1 |> answer_to_text

let part2 (input_text: string) : (string) =
  input_text |> text_to_input |> solve_part2 |> answer_to_text
