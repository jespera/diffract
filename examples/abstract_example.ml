(* Example: Finding similar changes across multiple diffs *)

(* Two pairs of files with similar changes *)
let before1 = {|
function hello(name: string): void {
  console.log("Hello, " + name);
}
|}

let after1 = {|
function hello(name: string): void {
  console.warn("Hello, " + name);
}
|}

let before2 = {|
function greet(user: string): void {
  console.log("Welcome, " + user);
}
|}

let after2 = {|
function greet(user: string): void {
  console.warn("Welcome, " + user);
}
|}

(* A different kind of change *)
let before3 = {|
function add(a: number, b: number): number {
  return a + b;
}
|}

let after3 = {|
function add(a: number, b: number, c: number): number {
  return a + b + c;
}
|}

let () =
  print_endline "=== Finding Similar Changes ===\n";

  (* Compute diffs *)
  let diff1 = Diffract.diff ~language:"typescript" ~before:before1 ~after:after1 in
  let diff2 = Diffract.diff ~language:"typescript" ~before:before2 ~after:after2 in
  let diff3 = Diffract.diff ~language:"typescript" ~before:before3 ~after:after3 in

  (* Flatten and abstract all changes *)
  let flat1 = Diffract.Diff.flatten_changes diff1.changes in
  let flat2 = Diffract.Diff.flatten_changes diff2.changes in
  let flat3 = Diffract.Diff.flatten_changes diff3.changes in

  (* Tag each change with its source for later reference *)
  let tag_changes result tag =
    List.map (fun c -> (tag, Diffract.Abstract.abstract_change result c))
  in

  let all_abstract =
    tag_changes diff1 "diff1" flat1 @
    tag_changes diff2 "diff2" flat2 @
    tag_changes diff3 "diff3" flat3
  in

  print_endline "All changes (abstracted):\n";
  List.iter (fun (tag, ac) ->
    Printf.printf "  [%s] %s\n" tag (Diffract.Abstract.to_string ac)
  ) all_abstract;

  (* Group by template - need to just get the abstract changes for grouping *)
  let just_abstract = List.map snd all_abstract in
  let groups = Diffract.Abstract.group_by_template just_abstract in

  print_endline "\n=== Grouped by Pattern ===\n";
  List.iteri (fun i group ->
    Printf.printf "Group %d (%d changes):\n" (i + 1) (List.length group);
    List.iter (fun ac ->
      Printf.printf "  %s\n" (Diffract.Abstract.to_string ac)
    ) group;
    print_newline ()
  ) groups
