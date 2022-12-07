open Base

type t = Empty
type fs_item = { name: string ; size: int option }
type fs_tree =
| Directory of ( fs_item * fs_tree list )
| File of fs_item
type input = Input of fs_tree
type answer = Answer of int | Unknown
type line_type = Cmd_cd  of string | Cmd_ls | Out_d of string | Out_f of int * string
exception Item_not_found of string

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
  | ".." -> path |> List.rev |> List.drop 1 |> List.rev
  | _ -> path |> List.rev |> List.cons dir |> List.rev

let rec add_fs_item (Directory (item, content)) (fsi: fs_item) (path: string list) : fs_tree =
  match path with
  | [] -> fsi
  | p::ps -> 
      let fltr = function 
      | Directory ( {name ; _}, _) -> String.(name = p)
      | _ -> false in
      let subdir = List.find content ~f:fltr in 
      let others = List.filter content ~f:(fun x -> not @@ fltr x) in 
      match subdir with 
      | Some x -> Directory ( item , (add_file ps file size x) :: others )
      | _ -> raise (Item_not_found p)

let parse_one_more_line line (path:string , fst: fs_tree) as acc =
  match line_type(line) with
  | Cmd_ls -> acc
  | Cmd_cd dir -> (cd path dir, fst)
  | Out_d dir -> 
      let fsi = Directory ({name = dir ; size = None}, []) in
      (path, add_fs_item fst fsi)
  | Out_f (size, file) -> 
      let fsi = File {name = file ; size = Some size} in
      (path, add_fs_item fst fsi)

let text_to_input (t: string) :input =
  let init = ([], Directory {name = "", size = None}) in
  t
  |> String.split_lines
  |> List.Fold ~init:init ~f:parse_one_more_line

(* Solution for part 1 *)
let part1 (Input _ : input) : answer = Unknown

(* Solution for part 1 *)
let part2 (Input _ : input) : answer = Unknown

let answer_to_text = function
  | Answer x -> Int.to_string x
  | Unknown  -> "Solution not yet implemented"
