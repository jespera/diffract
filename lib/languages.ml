(* Language registry - statically linked tree-sitter grammars *)

external typescript_language : unit -> nativeint = "tsh_typescript_language"
external tsx_language : unit -> nativeint = "tsh_tsx_language"
external kotlin_language : unit -> nativeint = "tsh_kotlin_language"
external php_language : unit -> nativeint = "tsh_php_language"
external scala_language : unit -> nativeint = "tsh_scala_language"

(* Canonical name, aliases, loader function *)
let canonical_info =
  [
    ("typescript", [ "ts" ], typescript_language);
    ("tsx", [], tsx_language);
    ("kotlin", [ "kt" ], kotlin_language);
    ("php", [], php_language);
    ("scala", [], scala_language);
  ]

let language_info =
  List.concat_map
    (fun (name, aliases, loader) ->
      (name, loader) :: List.map (fun a -> (a, loader)) aliases)
    canonical_info

let get (ctx : Context.t) name =
  let name_lower = String.lowercase_ascii name in
  match Hashtbl.find_opt ctx.lang_cache name_lower with
  | Some lang -> lang
  | None -> (
      match List.assoc_opt name_lower language_info with
      | Some loader ->
          let lang = loader () in
          Hashtbl.add ctx.lang_cache name_lower lang;
          lang
      | None ->
          let available =
            List.map fst language_info |> List.sort_uniq String.compare
          in
          failwith
            (Printf.sprintf "Unknown language: %s. Available: %s" name
               (String.concat ", " available)))

let list_available () =
  List.map
    (fun (name, aliases, _) ->
      match aliases with
      | [] -> name
      | _ -> Printf.sprintf "%s (aliases: %s)" name (String.concat ", " aliases))
    canonical_info
