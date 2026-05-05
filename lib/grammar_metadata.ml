(** Per-language grammar metadata derived from each tree-sitter
    grammar's [node-types.json]. *)

(** [parse_wrapper_set json_str] parses a [node-types.json] document
    and returns a hashtable whose keys are the node-type names of
    list-shape wrappers in that grammar.

    Criterion: [named: true] and [fields] is missing or empty. The
    runtime peel logic in [Tree.unwrap_root] adds the necessary
    structural guards (single-named-child, byte-range equality), so a
    slightly loose superset is fine here. *)
let parse_wrapper_set (json_str : string) : (string, unit) Hashtbl.t =
  let open Yojson.Safe.Util in
  let entries =
    try Yojson.Safe.from_string json_str |> to_list with _ -> []
  in
  let set = Hashtbl.create 64 in
  List.iter
    (fun entry ->
      let named =
        match member "named" entry with `Bool b -> b | _ -> false
      in
      let has_fields =
        match member "fields" entry with
        | `Assoc fs -> fs <> []
        | _ -> false
      in
      let type_name =
        match member "type" entry with `String s -> Some s | _ -> None
      in
      match type_name with
      | Some t when named && not has_fields -> Hashtbl.replace set t ()
      | _ -> ())
    entries;
  set

(** Per-language wrapper-set cache. Populated lazily; the embedded JSON
    string for a language is parsed at most once. *)
let cache : (string, (string, unit) Hashtbl.t) Hashtbl.t = Hashtbl.create 8

let load_language (language : string) : (string, unit) Hashtbl.t =
  match Hashtbl.find_opt cache language with
  | Some set -> set
  | None ->
      let set =
        match Embedded_metadata.by_language language with
        | None -> Hashtbl.create 0
        | Some json -> parse_wrapper_set json
      in
      Hashtbl.replace cache language set;
      set

let is_list_shape_wrapper ~language ~node_type =
  Hashtbl.mem (load_language language) node_type

let list_shape_wrappers ~language =
  let set = load_language language in
  Hashtbl.fold (fun k () acc -> k :: acc) set []
  |> List.sort String.compare

let all_languages () = [ "typescript"; "tsx"; "kotlin"; "php"; "scala" ]
