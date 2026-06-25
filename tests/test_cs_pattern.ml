(** Unit tests for the change-summary pattern layer ({!Diffract.Cs_pattern}),
    focused on Piece C: sequence (ellipsis) generalisation of delimited list
    children whose arity varies across instances.

    A delimited list (parameter / argument / type-argument list) that differs in
    arity between two anti-unified instances should generalise to [(...)] — the
    interior becomes a sequence wildcard, the brackets are kept as the anchor —
    rather than fragmenting per-arity or collapsing the whole node to one hole.
*)

open Diffract

let ctx = Context.create ()

let pat src =
  let t = Tree.parse ~ctx ~language:"kotlin" src in
  Cs_pattern.of_src src t.Tree.root

let render = Cs_pattern.render_pat_node
let au = Cs_pattern.anti_unify_pat

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  n = 0 || at 0

(* ── Parameter lists ──────────────────────────────────────────────── *)

let differing_arity_yields_ellipsis () =
  let r =
    render
      (au
         (pat "fun f(x: Int) { return x }")
         (pat "fun f(x: Int, y: Int) { return y }"))
  in
  Alcotest.(check bool)
    (Printf.sprintf "differing arity yields (...) — got %S" r)
    true
    (contains ~sub:"(...)" r)

let same_arity_keeps_holes_no_ellipsis () =
  (* Same arity (one param each), differing names: the param list recurses to
     per-element holes — no ellipsis is introduced. *)
  let r =
    render (au (pat "fun f(a: Int) { return a }") (pat "fun g(b: Int) { return b }"))
  in
  Alcotest.(check bool)
    (Printf.sprintf "same arity does not introduce '...' — got %S" r)
    false (contains ~sub:"..." r);
  Alcotest.(check bool) "parens preserved" true (contains ~sub:"(" r)

let single_return_body_preserved () =
  (* The property fun-exp relies on: bodies with the same statement count are
     NOT collapsed — the [return] stays visible so the conversion is expressible
     rather than ellipsised away. *)
  let r =
    render
      (au
         (pat "fun f(x: Int) { return x }")
         (pat "fun g(x: Int, y: Int) { return y }"))
  in
  Alcotest.(check bool)
    (Printf.sprintf "body keeps 'return' (not collapsed) — got %S" r)
    true (contains ~sub:"return" r)

let ellipsis_convergence_across_three_arities () =
  (* Once two instances generalise the param list to (...), anti-unifying with a
     third instance of yet another arity must KEEP (...), not regress to a bare
     hole — otherwise a dendrogram could not merge a whole arity-varying family. *)
  let e12 =
    au (pat "fun f(x: Int) { return x }") (pat "fun f(x: Int, y: Int) { return x }")
  in
  let e123 = au e12 (pat "fun f(a: Int, b: Int, c: Int) { return a }") in
  let r = render e123 in
  Alcotest.(check bool)
    (Printf.sprintf "convergence keeps (...) — got %S" r)
    true
    (contains ~sub:"(...)" r)

(* ── Call argument lists ──────────────────────────────────────────── *)

let call_arity_yields_ellipsis () =
  (* A call whose argument count varies generalises its argument list to (...). *)
  let r = render (au (pat "val z = foo(a)") (pat "val z = foo(a, b, c)")) in
  Alcotest.(check bool)
    (Printf.sprintf "call arity yields (...) — got %S" r)
    true
    (contains ~sub:"(...)" r)

(* ── Field-mode rendering (Piece B, increment 1) ──────────────────── *)

(* of_src the first top-level declaration in [src]. *)
let decl src =
  let t = Tree.parse ~ctx ~language:"kotlin" src in
  match t.Tree.root.Tree.children with
  | c :: _ -> Cs_pattern.of_src src c.Tree.node
  | [] -> failwith "no declaration"

let field_render_shape () =
  let ep =
    {
      Cs_types.before = decl "fun f(x: Int) { return x }";
      after = decl "fun f(x: Int) = x";
    }
  in
  let r = Cs_pattern.render_pattern_body_field ep in
  Alcotest.(check bool) (Printf.sprintf "match: field — got %S" r) true
    (contains ~sub:"match: field" r);
  Alcotest.(check bool) "signature is a context line" true
    (contains ~sub:"\n  fun f(x: Int)" r || contains ~sub:"  fun f(x: Int)" r);
  Alcotest.(check bool) "body removed" true (contains ~sub:"- {" r);
  Alcotest.(check bool) "expression body added" true (contains ~sub:"+ = x" r)

let field_render_round_trip () =
  (* The rendered field rule, applied to the before source, reproduces the
     after — the renderer's output is a usable rule. *)
  let bsrc = "fun f(x: Int) { return x }" in
  let asrc = "fun f(x: Int) = x" in
  let ep = { Cs_types.before = decl bsrc; after = decl asrc } in
  let r = Cs_pattern.render_pattern_body_field ep in
  let out =
    Matcher.transform ~ctx ~language:"kotlin" ~pattern_text:r ~source_text:bsrc
  in
  Alcotest.(check string)
    (Printf.sprintf "round-trips to after (rule=%S)" r)
    asrc (String.trim out)

(* ── Field-aware alignment (Piece B, increment 2) ─────────────────── *)

(* Merge two block→expression-body change-pairs whose function signatures differ
   only by return-type *presence*. Field-aware alignment should drop the
   non-shared return-type field and merge into one field rule, rather than
   collapsing the differing-child-count declaration to a bare hole. *)
let field_align_return_type_presence () =
  let ep1 =
    {
      Cs_types.before = decl "fun f(x: Int): Int { return x }";
      after = decl "fun f(x: Int): Int = x";
    }
  in
  let ep2 =
    {
      Cs_types.before = decl "fun g(y: Int) { return y }";
      after = decl "fun g(y: Int) = y";
    }
  in
  let r =
    Cs_pattern.render_pattern_body_field (Cs_pattern.anti_unify_edits ep1 ep2)
  in
  (* merged (not collapsed to a bare hole), field mode, body is the edit, and
     the return type is dropped (no `): Int` after the params). *)
  Alcotest.(check bool) (Printf.sprintf "field rule — got %S" r) true
    (contains ~sub:"match: field" r);
  Alcotest.(check bool) "signature survived as context (fun _H)" true
    (contains ~sub:"fun _H" r);
  Alcotest.(check bool) "body is the edit" true
    (contains ~sub:"- {" r && contains ~sub:"+ = " r);
  Alcotest.(check bool) "return type dropped" false (contains ~sub:"): Int" r)

(* The fun-exp shape: merge functions that differ in BOTH arity and return-type
   presence into one field rule with `(...)` params and the return type dropped —
   and confirm the merged rule actually applies (round-trip). *)
let field_align_arity_and_return_type () =
  let ep1 =
    {
      Cs_types.before = decl "fun f(x: Int): Int { return x }";
      after = decl "fun f(x: Int): Int = x";
    }
  in
  let ep2 =
    {
      Cs_types.before = decl "fun g() { return zero }";
      after = decl "fun g() = zero";
    }
  in
  let r =
    Cs_pattern.render_pattern_body_field (Cs_pattern.anti_unify_edits ep1 ep2)
  in
  Alcotest.(check bool) (Printf.sprintf "field rule — got %S" r) true
    (contains ~sub:"match: field" r);
  Alcotest.(check bool) "params generalised to (...)" true
    (contains ~sub:"(...)" r);
  Alcotest.(check bool) "return type dropped" false (contains ~sub:"): Int" r);
  (* round-trip: the merged rule converts a concrete site, preserving its own
     params and return type (field mode ignores the latter). *)
  let out =
    Matcher.transform ~ctx ~language:"kotlin" ~pattern_text:r
      ~source_text:"fun f(a: String, b: String): Foo { return bar }"
  in
  Alcotest.(check string)
    (Printf.sprintf "round-trips (rule=%S)" r)
    "fun f(a: String, b: String): Foo = bar" (String.trim out)

let tests =
  [
    ( "differing param arity yields (...)",
      `Quick,
      differing_arity_yields_ellipsis );
    ( "field-align: return-type presence variance",
      `Quick,
      field_align_return_type_presence );
    ( "field-align: arity + return-type (fun-exp shape)",
      `Quick,
      field_align_arity_and_return_type );
    ( "same arity keeps per-element holes",
      `Quick,
      same_arity_keeps_holes_no_ellipsis );
    ("single-return body preserved", `Quick, single_return_body_preserved);
    ( "ellipsis converges across three arities",
      `Quick,
      ellipsis_convergence_across_three_arities );
    ("call argument arity yields (...)", `Quick, call_arity_yields_ellipsis);
    ("field render: shape", `Quick, field_render_shape);
    ("field render: round-trips", `Quick, field_render_round_trip);
  ]
