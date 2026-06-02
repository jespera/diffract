open Diffract

type leaf = { text : string; node_type : string }

let rec get_leaves (source : string) (node : _ Tree.t) : leaf list =
  if node.Tree.is_extra && node.Tree.node_type <> "ERROR" then []
  else
    match node.Tree.children with
    | [] ->
        [ { text = Tree.text source node; node_type = node.Tree.node_type } ]
    | children ->
        List.concat_map
          (fun (c : _ Tree.child) -> get_leaves source c.node)
          children

let string_of_leaf (l : leaf) = Printf.sprintf "%s:%s" l.text l.node_type

let string_of_leaves (ls : leaf list) =
  "[" ^ String.concat "; " (List.map string_of_leaf ls) ^ "]"

let slice_leaves leaves strip_prefix strip_suffix =
  let rec drop n = function
    | [] -> []
    | xs when n <= 0 -> xs
    | _ :: xs -> drop (n - 1) xs
  in
  let rec take n = function
    | [] -> []
    | x :: xs when n > 0 -> x :: take (n - 1) xs
    | _ -> []
  in
  let dropped = drop strip_prefix leaves in
  take (List.length dropped - strip_suffix) dropped

type test_case = {
  language : string;
  pattern : string;
  wrapped : string;
  strip_prefix : int;
  strip_suffix : int;
}

let test_cases =
  [
    (* TypeScript *)
    {
      language = "typescript";
      pattern = {|foo / $x / g|};
      wrapped = {|function dummy() { foo / $x / g }|};
      strip_prefix = 5;
      (* function, dummy, (, ), { *)
      strip_suffix = 1;
      (* } *)
    };
    {
      language = "typescript";
      pattern = "} else {";
      wrapped = {|if (cond) { } else { }|};
      strip_prefix = 5;
      (* if, (, cond, ), { *)
      strip_suffix = 1;
      (* } *)
    };
    (* TSX *)
    {
      language = "tsx";
      pattern = {|<Foo>|};
      wrapped = {|const x = <Foo></Foo>|};
      strip_prefix = 3;
      (* const, x, = *)
      strip_suffix = 3;
      (* </, Foo, > *)
    };
    {
      language = "tsx";
      pattern = {|foo<Bar>|};
      wrapped = {|const x = foo<Bar>|};
      strip_prefix = 3;
      (* const, x, = *)
      strip_suffix = 0;
    };
    {
      language = "tsx";
      pattern = {|a < Foo > b|};
      wrapped = {|const x = a < Foo > b|};
      strip_prefix = 3;
      (* const, x, = *)
      strip_suffix = 0;
    };
    (* Kotlin *)
    {
      language = "kotlin";
      pattern = {|$"hello $name"|};
      wrapped = {|val x = $"hello $name"|};
      strip_prefix = 3;
      (* val, x, = *)
      strip_suffix = 0;
    };
    {
      language = "kotlin";
      pattern = {|$$$"foo $$$bar"|};
      wrapped = {|val x = $$$"foo $$$bar"|};
      strip_prefix = 3;
      (* val, x, = *)
      strip_suffix = 0;
    };
    (* PHP *)
    {
      language = "php";
      pattern = {|function $f() { $body|};
      wrapped = {|<?php function $f() { $body } ?>|};
      strip_prefix = 1;
      (* <?php *)
      strip_suffix = 2;
      (* }, ?> *)
    };
    (* Scala *)
    {
      language = "scala";
      pattern = {|s"hello $name"|};
      wrapped = {|val x = s"hello $name"|};
      strip_prefix = 3;
      (* val, x, = *)
      strip_suffix = 0;
    };
  ]

let run_test_case ctx tc =
  let raw_tree = Tree.parse ~ctx ~language:tc.language tc.pattern in
  let wrapped_tree = Tree.parse ~ctx ~language:tc.language tc.wrapped in
  let raw_leaves = get_leaves tc.pattern raw_tree.root in
  let wrapped_all_leaves = get_leaves tc.wrapped wrapped_tree.root in
  let wrapped_leaves =
    slice_leaves wrapped_all_leaves tc.strip_prefix tc.strip_suffix
  in
  if raw_leaves <> wrapped_leaves then
    Alcotest.failf
      "Mismatch for %s pattern '%s':\n\
       Raw leaves:     %s\n\
       Wrapped sliced: %s\n\
       Wrapped all:    %s"
      tc.language tc.pattern
      (string_of_leaves raw_leaves)
      (string_of_leaves wrapped_leaves)
      (string_of_leaves wrapped_all_leaves)

let make_test ctx tc =
  let name = Printf.sprintf "%s: %s" tc.language tc.pattern in
  Alcotest.test_case name `Quick (fun () -> run_test_case ctx tc)

let tests =
  let ctx = Context.create () in
  List.map (make_test ctx) test_cases
