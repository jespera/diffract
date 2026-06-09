(** Tests for Tree_inclusion — ordered tree embedding. *)

open Diffract

let ctx = Context.create ()

(* Parse [snippet] and return (source, first node of type [ty]). *)
let extract ?(lang = "typescript") ty snippet =
  let t = parse_tree ~ctx ~language:lang snippet in
  match Tree.find_by_type ty t.Tree.root with
  | n :: _ -> (t.Tree.source, n)
  | [] -> Alcotest.failf "no %s node in %S" ty snippet

let incl sub sup = Tree_inclusion.included_src ~sub ~sup

let check name expected sub sup =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (incl sub sup))

let tests =
  [
    (* reflexive *)
    (let x = extract "identifier" "x;" in
     check "x included in x" true x x);
    (let c = extract "call_expression" "f(x);" in
     check "f(x) included in f(x)" true c c);
    (* internal-node deletion (promotion): x ⊑ x + 1 *)
    (let x = extract "identifier" "x;" in
     let e = extract "binary_expression" "x + 1;" in
     check "x included in x+1 (delete +1)" true x e);
    (* the reverse is not a deletion *)
    (let x = extract "identifier" "x;" in
     let e = extract "binary_expression" "x + 1;" in
     check "x+1 not included in x" false e x);
    (* empty container ⊑ populated (insertion residual) *)
    (let empty = extract "array" "[];" in
     let full = extract "array" "[b, c];" in
     check "[] included in [b, c]" true empty full);
    (let empty = extract "array" "[];" in
     let full = extract "array" "[b, c];" in
     check "[b, c] not included in []" false full empty);
    (* deeper promotion: [b, d] ⊑ [a(b, d)] (delete the call wrapper) *)
    (let bd = extract "array" "[b, d];" in
     let wrapped = extract "array" "[a(b, d)];" in
     check "[b,d] included in [a(b,d)]" true bd wrapped);
    (* descend to a sub-expression: x ⊑ f(x) *)
    (let x = extract "identifier" "x;" in
     let fx = extract "call_expression" "f(x);" in
     check "x included in f(x)" true x fx);
    (* a relabel is not an inclusion (the detour case) *)
    (let g = extract "call_expression" "g(x);" in
     let h = extract "call_expression" "h(x);" in
     check "g(x) not included in h(x)" false g h);
    (* order sensitivity: [a, b] does not embed in [b, a] *)
    (let ab = extract "array" "[a, b];" in
     let ba = extract "array" "[b, a];" in
     check "[a,b] not included in [b,a] (ordered)" false ab ba);
    (* but it does embed in an order-preserving superset *)
    (let ab = extract "array" "[a, b];" in
     let axb = extract "array" "[a, x, b];" in
     check "[a,b] included in [a,x,b]" true ab axb);
    (* leaf text matters: distinct identifiers don't match *)
    (let x = extract "identifier" "x;" in
     let y = extract "identifier" "y;" in
     check "x not included in y" false x y);
  ]
