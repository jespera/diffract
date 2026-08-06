(** Tests for the reading-mode residual digest ({!Diffract.Cs_group}).

    The digest never changes what a summary {e means} — only how
    [--format text-minimal] prints it — so these tests pin the two things that
    decide whether the print is trustworthy: which hunks are judged "the same
    edit", and that every hunk survives exactly once. *)

open Diffract

let sig_of a b = Cs_group.signature a b
let desc a b = Cs_group.describe (sig_of a b)

let check_desc name a b expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (desc a b))

let same_edit name a b c d =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool)
        (Printf.sprintf "%S/%S and %S/%S group together" a b c d)
        true
        (sig_of a b = sig_of c d))

let differs name a b c d =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool)
        (Printf.sprintf "%S/%S and %S/%S stay apart" a b c d)
        false
        (sig_of a b = sig_of c d))

(* ── signatures ──────────────────────────────────────────────────── *)

let signature_tests =
  [
    (* The grouping property: same edit, different surrounding code. *)
    same_edit "same edit in different context"
      "val a = \"akka.cluster.AutoDowning\"" "val a = \"org.apache.pekko.cluster.AutoDowning\""
      "if (x == \"akka.cluster.Other\")" "if (x == \"org.apache.pekko.cluster.Other\")";
    (* ...and the converse, or the digest would merge unrelated changes. *)
    differs "different edits stay apart" "import akka.actor.X"
      "import pekko.actor.X" "import akka.actor.X" "import org.apache.pekko.actor.X";
    check_desc "replacement" "a.akka.b" "a.pekko.b" "akka -> pekko";
    check_desc "pure insertion" "" "import Foo" "(insert) import Foo";
    check_desc "pure deletion" "extends Bar" "" "(delete) extends Bar";
    (* A lone separator between two changed runs must not split one rename
       into "akka -> org" plus "insert apache/pekko/" — the defect that made
       the first pekko render unreadable. *)
    check_desc "separator bridges a split rename"
      "src/akka/cluster/X.scala" "src/org/apache/pekko/cluster/X.scala"
      "akka/ -> org/apache/pekko/";
    (* A kept *word* is real context and must still end the segment. *)
    Alcotest.test_case "kept word ends a segment" `Quick (fun () ->
        Alcotest.(check int)
          "two separate edits on one line" 2
          (List.length (sig_of "a X b Y c" "a P b Q c")));
    (* Reflow alongside a real change groups with the unreflowed form. *)
    same_edit "whitespace-only runs ignored" "foo(a,  b)" "bar(a,  b)" "foo(a, b)"
      "bar(a, b)";
    Alcotest.test_case "identical text has no signature" `Quick (fun () ->
        Alcotest.(check bool) "empty" true (sig_of "same" "same" = []));
    (* Identifier-internal renames are reported literally, once per
       identifier: the digest knows no naming conventions, so these do NOT
       group. Pinned because grouping them is a tempting change that would
       quietly bake camelCase into the tool. *)
    check_desc "identifier-internal rename stays literal"
      "class FooLibraryService" "class FooService"
      "FooLibraryService -> FooService";
    differs "no grouping across identifiers" "FooLibraryService" "FooService"
      "BarLibraryModule" "BarModule";
    (* Doing one edit twice is the same change as doing it once, so the two
       must land in one group rather than two. *)
    same_edit "repeated edit collapses" "a akka b akka c" "a pekko b pekko c"
      "x akka y" "x pekko y";
    (* ...but two *different* edits are still two. *)
    Alcotest.test_case "distinct edits both survive collapse" `Quick (fun () ->
        Alcotest.(check int)
          "two segments" 2
          (List.length (sig_of "a akka b foo c" "a pekko b bar c")));
    (* Multi-byte text must not be cut mid-character by the eliding renderer.
       Three-byte characters, so the 40-byte cut lands mid-character and the
       backoff has to do real work. *)
    Alcotest.test_case "long unicode edit elides at a char boundary" `Quick
      (fun () ->
        let long = String.concat "" (List.init 40 (fun _ -> "\xe3\x81\x82")) in
        let d = desc long "x" in
        let ell = "\xe2\x80\xa6" in
        let kept =
          (* the removed side, up to the ellipsis the truncation appends *)
          let rec find i =
            if i + 3 > String.length d then None
            else if String.sub d i 3 = ell then Some i
            else find (i + 1)
          in
          match find 0 with Some i -> String.sub d 0 i | None -> d
        in
        Alcotest.(check bool)
          "truncated on a character boundary" true
          (String.length kept mod 3 = 0);
        Alcotest.(check bool)
          "and actually truncated" true
          (String.length kept < 120));
  ]

(* ── the digest over residuals ───────────────────────────────────── *)

let residual ?moved file diff : Cs_types.residual =
  { res_file = file; res_moved_to = moved; res_rules = []; res_diff = diff }

let hunk_diff file body =
  Printf.sprintf "--- a/%s\n+++ b/%s\n%s" file file body

(* Two files whose single hunk is the same edit, plus one that differs. *)
let sample =
  [
    residual "a.scala"
      (hunk_diff "a.scala" "@@ -1,1 +1,1 @@\n-val x = akka.A\n+val x = pekko.A\n");
    residual "b.scala"
      (hunk_diff "b.scala" "@@ -3,1 +3,1 @@\n-val y = akka.B\n+val y = pekko.B\n");
    residual "c.scala"
      (hunk_diff "c.scala" "@@ -9,1 +9,1 @@\n-val z = 1\n+val z = 2\n");
  ]

let digest_tests =
  [
    Alcotest.test_case "repeated edit groups, singleton does not" `Quick
      (fun () ->
        let d = Cs_group.digest sample in
        Alcotest.(check int) "one group" 1 (List.length d.dg_groups);
        let g = List.hd d.dg_groups in
        Alcotest.(check int) "covering two hunks" 2 g.g_count;
        Alcotest.(check int) "in two files" 2 g.g_files;
        Alcotest.(check string) "described" "akka -> pekko" g.g_edit);
    (* The accounting the reader relies on: nothing is silently dropped. *)
    Alcotest.test_case "every hunk appears exactly once" `Quick (fun () ->
        let d = Cs_group.digest sample in
        let rest = List.concat_map snd d.dg_rest in
        Alcotest.(check int) "three hunks seen" 3 d.dg_total;
        Alcotest.(check int)
          "grouped + ungrouped = total" d.dg_total
          (d.dg_grouped + List.length rest);
        Alcotest.(check int) "only the odd one out is printed" 1
          (List.length rest));
    Alcotest.test_case "no renames when nothing moved" `Quick (fun () ->
        let d = Cs_group.digest sample in
        Alcotest.(check bool) "none" true (d.dg_renames = None));
    (* A moved file counts as a rename even when it also has content left
       over — counting only content-free moves reported 86 of pekko's 131. *)
    Alcotest.test_case "moved-with-content still counts as a rename" `Quick
      (fun () ->
        let moved =
          [
            residual ~moved:"new/a.scala" "old/a.scala"
              (hunk_diff "old/a.scala" "@@ -1,1 +1,1 @@\n-akka\n+pekko\n");
            residual ~moved:"new/b.scala" "old/b.scala"
              "similarity index 100%\nrename from old/b.scala\nrename to new/b.scala\n";
          ]
        in
        let d = Cs_group.digest moved in
        match d.dg_renames with
        | None -> Alcotest.fail "expected a rename digest"
        | Some (files, edits) ->
            Alcotest.(check int) "both files counted" 2 files;
            Alcotest.(check int) "one shared path edit" 1 (List.length edits);
            Alcotest.(check int) "with both occurrences" 2
              (List.hd edits).re_count);
  ]

let tests = signature_tests @ digest_tests
