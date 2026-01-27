(* Example: Anti-unification to find common change patterns *)

(* Small examples that demonstrate the key transformations *)
let snippet1_before = {|console.log("Fetching user: " + id);|}
let snippet1_after = {|this.logger.info("Fetching user: " + id);|}

let snippet2_before = {|console.log("User not found: " + id);|}
let snippet2_after = {|this.logger.warn("User not found: " + id);|}

let snippet3_before = {|console.log("Creating order for user: " + userId);|}
let snippet3_after = {|this.logger.info("Creating order for user: " + userId);|}

let snippet4_before = {|console.log("Order cancelled: " + orderId);|}
let snippet4_after = {|this.logger.info("Order cancelled: " + orderId);|}

(* Collect the deepest leaf changes only (avoid duplicates from nested structure) *)
let rec collect_deepest_changes ann =
  match ann with
  | Diffract.Antiunify.Same _ -> []
  | Diffract.Antiunify.Diff _ -> [ann]
  | Diffract.Antiunify.Added _ -> [ann]
  | Diffract.Antiunify.Removed _ -> [ann]
  | Diffract.Antiunify.Replaced _ -> [ann]
  | Diffract.Antiunify.Modified { children; _ } ->
    List.concat_map collect_deepest_changes children

(* Deduplicate changes by their string representation *)
let dedupe_changes changes =
  let seen = Hashtbl.create 16 in
  List.filter (fun ann ->
    let s = Diffract.Antiunify.to_string ann in
    if Hashtbl.mem seen s then false
    else begin Hashtbl.add seen s (); true end
  ) changes

let () =
  let snippets = [
    ("Snippet 1", snippet1_before, snippet1_after);
    ("Snippet 2", snippet2_before, snippet2_after);
    ("Snippet 3", snippet3_before, snippet3_after);
    ("Snippet 4", snippet4_before, snippet4_after);
  ] in

  print_endline "=== Individual changes (showing what differs) ===\n";

  let all_leaf_changes = List.concat_map (fun (name, before, after) ->
    Printf.printf "%s:\n" name;
    Printf.printf "  Before: %s\n" before;
    Printf.printf "  After:  %s\n" after;

    let diff = Diffract.diff ~language:"typescript" ~before ~after in
    let flat = Diffract.Diff.flatten_changes diff.changes in

    (* Get the top-level annotated change *)
    let top_ann = match flat with
      | change :: _ -> Some (Diffract.Antiunify.antiunify_change diff change)
      | [] -> None
    in

    (* Show the annotated version *)
    (match top_ann with
    | Some ann ->
      Printf.printf "  Change: %s\n" (Diffract.Antiunify.to_string ann)
    | None -> ());

    (* Collect unique deepest leaf changes *)
    let leaves = match top_ann with
      | Some ann -> dedupe_changes (collect_deepest_changes ann)
      | None -> []
    in
    Printf.printf "  Leaf changes:\n";
    List.iter (fun leaf ->
      Printf.printf "    %s\n" (Diffract.Antiunify.to_string leaf)
    ) leaves;
    print_newline ();
    leaves
  ) snippets in

  print_endline "=== Finding common patterns across all changes ===\n";

  (* Group leaf changes by their structure *)
  let groups = Hashtbl.create 8 in
  List.iter (fun ann ->
    let key = match ann with
      | Diffract.Antiunify.Diff { node_type; _ } -> "diff:" ^ node_type
      | Diffract.Antiunify.Added { node_type; _ } -> "added:" ^ node_type
      | Diffract.Antiunify.Removed { node_type; _ } -> "removed:" ^ node_type
      | Diffract.Antiunify.Replaced { before_type; after_type; _ } ->
        "replaced:" ^ before_type ^ "->" ^ after_type
      | _ -> "other"
    in
    let existing = try Hashtbl.find groups key with Not_found -> [] in
    Hashtbl.replace groups key (ann :: existing)
  ) all_leaf_changes;

  Printf.printf "Total leaf changes: %d\n" (List.length all_leaf_changes);
  Printf.printf "Grouped into %d categories:\n\n" (Hashtbl.length groups);

  Hashtbl.iter (fun key changes ->
    Printf.printf "Category: %s (%d instances)\n" key (List.length changes);

    (* Show individual instances *)
    print_endline "  Instances:";
    List.iter (fun ann ->
      Printf.printf "    %s\n" (Diffract.Antiunify.to_string ann)
    ) changes;

    (* Anti-unify within this group *)
    let patterns = List.map Diffract.Antiunify.pattern_of_annotated changes in
    (match Diffract.Antiunify.antiunify_pattern_list patterns with
    | None -> ()
    | Some pattern ->
      Printf.printf "  Common pattern: %s\n" (Diffract.Antiunify.pattern_to_string pattern);
      for i = 0 to 3 do
        let vals = Diffract.Antiunify.get_variable_values i pattern in
        if vals <> [] then
          Printf.printf "    $%d = {%s}\n" i (String.concat ", " (List.sort_uniq compare vals))
      done);
    print_newline ()
  ) groups
