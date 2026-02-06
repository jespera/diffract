(* Language registry - loads tree-sitter grammars dynamically *)

open Ctypes
open Foreign

(* Map of language names to library base names and function names.
   Only includes languages with grammars built in grammars/lib/. *)
let language_info = [
  ("typescript", ("tree-sitter-typescript", "tree_sitter_typescript"));
  ("ts", ("tree-sitter-typescript", "tree_sitter_typescript"));
  ("tsx", ("tree-sitter-tsx", "tree_sitter_tsx"));
  ("kotlin", ("tree-sitter-kotlin", "tree_sitter_kotlin"));
  ("kt", ("tree-sitter-kotlin", "tree_sitter_kotlin"));
  ("php", ("tree-sitter-php", "tree_sitter_php"));
  ("scala", ("tree-sitter-scala", "tree_sitter_scala"));
]

(* Search paths for grammar libraries *)
let library_search_paths =
  let base_paths = [
    (* Relative to executable - for development *)
    "../grammars/lib";
    "grammars/lib";
    (* Relative to current directory *)
    "./grammars/lib";
    (* From _build directory (for tests) *)
    "../../../grammars/lib";
    "../../grammars/lib";
  ] in
  let env_path = match Sys.getenv_opt "DIFFRACT_GRAMMAR_PATH" with
    | Some p when p <> "" -> [p]
    | _ -> []
  in
  (* Standard system paths (empty string means use default search) *)
  base_paths @ env_path @ [""]

let try_load_library lib_basename =
  let lib_name = "lib" ^ lib_basename ^ ".so" in
  let rec try_paths = function
    | [] -> None
    | path :: rest ->
      let full_path = if path = "" then lib_name else Filename.concat path lib_name in
      try
        let lib = Dl.dlopen ~filename:full_path ~flags:[Dl.RTLD_NOW] in
        Some lib
      with Dl.DL_error _ -> try_paths rest
  in
  try_paths library_search_paths

let load_language lib_basename func_name =
  match try_load_library lib_basename with
  | None ->
    failwith (Printf.sprintf "Could not find library for %s. \
      Make sure grammar libraries are built (run grammars/build-grammars.sh) \
      or install system packages." lib_basename)
  | Some lib ->
    try
      (* The language function takes void and returns a pointer to TSLanguage *)
      let fn = foreign func_name (void @-> returning (ptr void)) ~from:lib in
      let lang_ptr = fn () in
      (* Convert ptr void to nativeint *)
      raw_address_of_ptr (to_voidp lang_ptr)
    with exn ->
      failwith (Printf.sprintf "Failed to load symbol %s: %s" func_name (Printexc.to_string exn))

let can_load_language lib_basename func_name =
  match try_load_library lib_basename with
  | None -> false
  | Some lib ->
    let ok =
      try
        let _ = Dl.dlsym ~handle:lib ~symbol:func_name in
        true
      with Dl.DL_error _ -> false
    in
    (try Dl.dlclose ~handle:lib with _ -> ());
    ok

let get (ctx : Context.t) name =
  let name_lower = String.lowercase_ascii name in
  match Hashtbl.find_opt ctx.lang_cache name_lower with
  | Some lang -> lang
  | None ->
    match List.assoc_opt name_lower language_info with
    | Some (lib_basename, func_name) ->
      let lang = load_language lib_basename func_name in
      Hashtbl.add ctx.lang_cache name_lower lang;
      lang
    | None ->
      let available = List.map fst language_info
                      |> List.sort_uniq String.compare in
      let available_str = String.concat ", " available in
      failwith (Printf.sprintf "Unknown language: %s. Available: %s" name available_str)

let list_available () =
  (* Group aliases by library and show canonical name with aliases. *)
  let groups : (string, (string * string * string list)) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  List.iter (fun (name, (lib_basename, func_name)) ->
    match Hashtbl.find_opt groups lib_basename with
    | None ->
      Hashtbl.add groups lib_basename (func_name, name, []);
      order := lib_basename :: !order
    | Some (fn, canonical, aliases) ->
      Hashtbl.replace groups lib_basename (fn, canonical, aliases @ [name])
  ) language_info;
  let order = List.rev !order in
  order
  |> List.filter_map (fun lib_basename ->
       match Hashtbl.find_opt groups lib_basename with
       | None -> None
       | Some (func_name, canonical, aliases) ->
         if can_load_language lib_basename func_name then
           let label =
             match aliases with
             | [] -> canonical
             | _ ->
               Printf.sprintf "%s (aliases: %s)"
                 canonical (String.concat ", " aliases)
           in
           Some label
         else
           None)
