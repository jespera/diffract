(* Language registry - loads tree-sitter grammars dynamically *)

open Ctypes
open Foreign

(* Cache for loaded languages *)
let loaded_languages : (string, nativeint) Hashtbl.t = Hashtbl.create 16

(* Map of language names to library base names and function names *)
let language_info = [
  ("typescript", ("tree-sitter-typescript", "tree_sitter_typescript"));
  ("ts", ("tree-sitter-typescript", "tree_sitter_typescript"));
  ("kotlin", ("tree-sitter-kotlin", "tree_sitter_kotlin"));
  ("kt", ("tree-sitter-kotlin", "tree_sitter_kotlin"));
  ("javascript", ("tree-sitter-javascript", "tree_sitter_javascript"));
  ("js", ("tree-sitter-javascript", "tree_sitter_javascript"));
  ("python", ("tree-sitter-python", "tree_sitter_python"));
  ("py", ("tree-sitter-python", "tree_sitter_python"));
  ("c", ("tree-sitter-c", "tree_sitter_c"));
  ("cpp", ("tree-sitter-cpp", "tree_sitter_cpp"));
  ("rust", ("tree-sitter-rust", "tree_sitter_rust"));
  ("go", ("tree-sitter-go", "tree_sitter_go"));
  ("java", ("tree-sitter-java", "tree_sitter_java"));
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

let get name =
  let name_lower = String.lowercase_ascii name in
  match Hashtbl.find_opt loaded_languages name_lower with
  | Some lang -> lang
  | None ->
    match List.assoc_opt name_lower language_info with
    | Some (lib_basename, func_name) ->
      let lang = load_language lib_basename func_name in
      Hashtbl.add loaded_languages name_lower lang;
      lang
    | None ->
      let available = List.map fst language_info
                      |> List.sort_uniq String.compare in
      let available_str = String.concat ", " available in
      failwith (Printf.sprintf "Unknown language: %s. Available: %s" name available_str)

let list_available () =
  (* Only list languages whose libraries are actually loadable, deduplicated by library *)
  language_info
  |> List.filter_map (fun (name, (lib_basename, _)) ->
       match try_load_library lib_basename with
       | Some _ -> Some (lib_basename, name)
       | None -> None)
  |> List.sort_uniq (fun (lib1, _) (lib2, _) -> String.compare lib1 lib2)
  |> List.map snd
