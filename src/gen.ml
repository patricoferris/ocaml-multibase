let read_file f =
  let ic = open_in f in
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    List.rev !lines
  with End_of_file -> List.rev !lines

type t = { name : string; code : string; description : string; status : string }

let from_file f =
  let from_line s =
    match String.split_on_char ',' s with
    | [ name; code; description; status ] ->
        {
          name = String.trim name;
          code = String.trim code;
          status = String.trim status;
          description = String.trim description;
        }
    | _ -> failwith "Too many elements in row"
  in
  match read_file f with
  | _ :: data ->
      let rec aux acc = function
        | [] -> List.rev acc
        | x :: xs -> aux (from_line x :: acc) xs
      in
      aux [] data
  | _ -> failwith "Struggling to read CSV... help!"

let string_to_pv s =
  String.split_on_char '-' s |> String.concat "_" |> String.capitalize_ascii
  |> ( ^ ) "`"

let pp_type_pv ppf (t, ts) =
  Fmt.pf ppf "type %s = [ %a ]\n\n" t
    Fmt.(list ~sep:(fun ppf _ -> Fmt.pf ppf " | ") string)
    (List.map string_to_pv ts)

let pp_to_code typ ppf ncs =
  let f ppf (name, code) = Fmt.pf ppf "%s -> %s\n" (string_to_pv name) code in
  Fmt.pf ppf "let %s_to_code = function\n%a" typ
    Fmt.(list ~sep:(fun ppf _ -> pf ppf "\n| ") f)
    ncs

let pp_of_code typ ppf ncs =
  let f ppf (name, code) =
    Fmt.pf ppf "%s -> Some %s\n" code (string_to_pv name)
  in
  Fmt.pf ppf "let %s_of_code = function\n%a| _ -> None\n\n" typ
    Fmt.(list ~sep:(fun ppf _ -> pf ppf "\n| ") f)
    ncs

let pp_to_string typ ppf ncs =
  let f ppf (name, _) = Fmt.pf ppf "%s -> \"%s\"\n" (string_to_pv name) name in
  Fmt.pf ppf "let %s_to_string = function\n%a\n\n" typ
    Fmt.(list ~sep:(fun ppf _ -> pf ppf "\n| ") f)
    ncs

let pp_of_string typ ppf ncs =
  let f ppf (name, _) =
    Fmt.pf ppf "\"%s\" -> Some %s\n" name (string_to_pv name)
  in
  Fmt.pf ppf "let %s_of_string = function\n%a\n | _ -> None\n" typ
    Fmt.(list ~sep:(fun ppf _ -> pf ppf "\n| ") f)
    ncs

let () =
  let mli = Sys.argv.(1) = "mli" in
  let data = from_file Sys.argv.(2) in
  let pvs = List.map (fun v -> v.name) data in
  let tags = List.map (fun v -> (string_to_pv v.name, v.code)) data in
  let names = List.map (fun v -> (string_to_pv v.name, v.name)) data in
  Fmt.pr "%a" pp_type_pv ("t", pvs);
  if mli then Fmt.pr "val to_code : t -> string\n"
  else
    Fmt.pr "\nlet to_code : t -> string = function\n%a%!\n"
      Fmt.(list (fun ppf (v, c) -> pf ppf "| %s -> \"%s\"" v c))
      tags;
  if mli then Fmt.pr "val of_code : string -> t option\n"
  else (
    Fmt.pr "\nlet of_code : string -> t option = function\n%a%!\n"
      Fmt.(list (fun ppf (v, c) -> pf ppf "| \"%s\" -> Some %s" c v))
      tags;
    Fmt.pr "| _ -> None\n\n");
  if mli then Fmt.pr "val to_string : t -> string\n"
  else
    Fmt.pr "\nlet to_string : t -> string = function\n%a%!\n"
      Fmt.(list (fun ppf (v, c) -> pf ppf "| %s -> \"%s\"" v c))
      names;
  if mli then Fmt.pr "val of_string : string -> t option"
  else (
    Fmt.pr "\nlet of_string : string -> t option = function\n%a%!\n"
      Fmt.(list (fun ppf (v, c) -> pf ppf "| \"%s\" -> Some %s" c v))
      names;
    Fmt.pr "\n| _ -> None")
