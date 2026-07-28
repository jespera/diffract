(** Tests for Leaf_metric — the token-level edit metric and the geodesic
    betweenness predicate (geodesic-gate plan, Phase 2). *)

open Diffract

let ctx = Context.create ()

(* Leaf stream of a snippet. *)
let lv ?(lang = "typescript") snippet =
  let t = parse_tree ~ctx ~language:lang snippet in
  Leaf_metric.leaves ~source:t.Tree.source t.Tree.root

let check_dist name expected a b =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check int) name expected (Leaf_metric.distance (lv a) (lv b));
      Alcotest.(check int)
        (name ^ " (symmetric)") expected
        (Leaf_metric.distance (lv b) (lv a)))

(* Check [geodesic] both without and with a caller-cached endpoint
   distance — the two paths must agree. *)
let check_geo name expected ~before ~mid ~after =
  Alcotest.test_case name `Quick (fun () ->
      let before = lv before and mid = lv mid and after = lv after in
      Alcotest.(check bool)
        name expected
        (Leaf_metric.geodesic ~before ~mid ~after ());
      Alcotest.(check bool)
        (name ^ " (cached endpoints)")
        expected
        (Leaf_metric.geodesic
           ~d_endpoints:(Leaf_metric.distance before after)
           ~before ~mid ~after ()))

let tests =
  [
    (* ── distance: identity and empty edges ── *)
    check_dist "identical source" 0 "f(x);" "f(x);";
    check_dist "empty vs empty" 0 "" "";
    check_dist "empty vs f();" 4 (* f ( ) ; *) "" "f();";
    (* ── formatting neutrality: whitespace is not a leaf ── *)
    check_dist "layout-only difference" 0 "f(x);" "f  (\n    x\n) ;";
    (* ── content differences count, one token = one delete + one insert ── *)
    check_dist "leaf flip" 2 "f(x);" "g(x);";
    check_dist "comment text counts" 2 "f(x); // a" "f(x); // b";
    (* ── geodesic: identity edges ── *)
    check_geo "mid = before" true ~before:"f(1); g(2);" ~mid:"f(1); g(2);"
      ~after:"f(9); h(2);";
    check_geo "mid = after" true ~before:"f(1); g(2);" ~mid:"f(9); h(2);"
      ~after:"f(9); h(2);";
    check_geo "identity site: mid = endpoints" true ~before:"f(1);" ~mid:"f(1);"
      ~after:"f(1);";
    check_geo "identity site: mid deviates" false ~before:"f(1);" ~mid:"f(2);"
      ~after:"f(1);";
    (* ── composite subset: applying one of two independent changes ── *)
    check_geo "composite subset (first change)" true ~before:"f(1); g(2);"
      ~mid:"f(9); g(2);" ~after:"f(9); h(2);";
    check_geo "composite subset (second change)" true ~before:"f(1); g(2);"
      ~mid:"f(1); h(2);" ~after:"f(9); h(2);";
    (* ── mangler: mid writes a value in neither endpoint ── *)
    check_geo "mangler (invented token)" false ~before:"f(1); g(2);"
      ~mid:"f(7); g(2);" ~after:"f(9); h(2);";
    (* ── intra-node partial step: a single leaf flip inside a bigger
       change. Tree inclusion calls this a relabel (a detour) in both
       directions; the metric admits it — the case the geodesic gate
       exists to recover. ── *)
    check_geo "intra-line leaf flip" true ~before:"const x = f(1);"
      ~mid:"const y = f(1);" ~after:"const y = g(1);";
    (* ── delete-then-readd, same position: off-geodesic. Each token the
       rule deletes and the residual must re-add in place pays twice
       (emptied-body soak shape). Inclusion blessed this as pure insertion
       and only net_progress caught it; the metric rejects it outright. ── *)
    check_geo "delete-then-readd in place" false ~before:"a(); b();" ~mid:"a();"
      ~after:"a(); b(); c();";
    (* ── delete-then-readd as a move: ON the geodesic. The change moves
       [a] to the back; a mid that has only deleted [a] sits between the
       endpoints. Policing wasted-but-metric-neutral work stays
       net_progress's job — this test documents the division of labour. ── *)
    check_geo "delete-then-readd of a moved element" true ~before:"[a, b, c];"
      ~mid:"[b, c];" ~after:"[b, c, a];";
    (* ── distance_upto: the cutoff ── *)
    Alcotest.test_case "distance_upto bounds" `Quick (fun () ->
        let a = lv "f(x);" and b = lv "g(y);" in
        Alcotest.(check int) "full distance" 4 (Leaf_metric.distance a b);
        Alcotest.(check (option int))
          "bound met" (Some 4)
          (Leaf_metric.distance_upto ~bound:4 a b);
        Alcotest.(check (option int))
          "bound exceeded" None
          (Leaf_metric.distance_upto ~bound:3 a b);
        Alcotest.(check (option int))
          "negative bound" None
          (Leaf_metric.distance_upto ~bound:(-1) a b);
        Alcotest.(check (option int))
          "zero bound, equal streams" (Some 0)
          (Leaf_metric.distance_upto ~bound:0 a a));
  ]
