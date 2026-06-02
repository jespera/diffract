(* A small tour of the tokenizer-based matcher on Kotlin. Note that metavars
   are SIGIL-FREE: in Kotlin `$` is real syntax (string templates), so a
   metavar is just its declared name (CLASS, MSG, ...), written without `$`. *)

open Diffract

let ctx = Context.create ()
let lang = "kotlin"

(* Print the source slice of every section of every match. *)
let show label ~source matches =
  Printf.printf "=== %s ===\n" label;
  if matches = [] then print_endline "  (no matches)"
  else
    List.iter
      (fun (c : Matcher.composite_match) ->
        List.iter
          (fun (r : Matcher.M.match_result) ->
            let span =
              String.sub source r.start_byte (r.end_byte - r.start_byte)
            in
            (* collapse newlines for a one-line preview *)
            let span =
              String.map (fun ch -> if ch = '\n' then ' ' else ch) span
            in
            Printf.printf "  %s\n" span)
          c.sections)
      matches;
  print_newline ()

let example_find () =
  (* Find every println call and bind its argument. *)
  let pattern = "@@\nmatch: strict\nmetavar MSG: single\n@@\nprintln(MSG)" in
  let source =
    "fun f() {\n\
    \    println(\"hello\")\n\
    \    log(\"skip me\")\n\
    \    println(greeting)\n\
     }"
  in
  show "find: println(MSG)" ~source
    (Matcher.find ~ctx ~language:lang ~pattern_text:pattern ~source_text:source)

let example_scoped () =
  (* Multi-section: a guard section matches the @Service class and binds its
     body; the second section, scoped `on BODY`, finds println only inside it.
     So println in a non-@Service class is not reported. *)
  let pattern =
    "@@\n\
     match: strict\n\
     metavar CLASS: single\n\
     metavar BODY: single\n\
     @@\n\
     @Service class CLASS { BODY }\n\
     @@\n\
     match: strict\n\
     on BODY\n\
     metavar MSG: single\n\
     @@\n\
     println(MSG)"
  in
  let source =
    "@Service class A { fun g() { println(\"in service\") } }\n\
     class B { fun h() { println(\"not in service\") } }"
  in
  show "find: println only inside @Service class (on BODY)" ~source
    (Matcher.find ~ctx ~language:lang ~pattern_text:pattern ~source_text:source)

let example_transform () =
  (* Rename println -> logger.info across the file. *)
  let pattern =
    "@@\n\
     match: strict\n\
     metavar MSG: single\n\
     @@\n\
     - println(MSG)\n\
     + logger.info(MSG)"
  in
  let source = "fun f() { println(a); println(b) }" in
  Printf.printf "=== transform: println -> logger.info ===\n  %s\n\n"
    (Matcher.transform ~ctx ~language:lang ~pattern_text:pattern
       ~source_text:source)

let () =
  example_find ();
  example_scoped ();
  example_transform ()
