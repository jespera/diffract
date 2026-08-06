(** Change-summary I/O: render a {!Cs_types.summary} to the [.summary] text
    format ([format_summary]) and load a {!Cs_types.changeset} by pairing two
    directory trees ([load_from_dirs]). Depends on {!Cs_types}, plus {!Tree} for
    the one shared line-range renderer. *)

open Cs_types

(* Abbreviated hard in the text format — this rides on an existing header line,
   where the reader only needs enough to see whether the hunk below sits in a
   damaged region. The JSON output carries every range. *)
let format_ranges = Tree.format_regions ~max_shown:3

let format_summary ?(sites = `Full) (s : summary) : string =
  let buf = Buffer.create 256 in
  List.iteri
    (fun i (r : rule) ->
      if i > 0 then Buffer.add_char buf '\n';
      (* §9.3 tier attribution: [after=] in the header when every site
         has the same predecessors, per-site annotations otherwise (a
         common-factor rule follows different primaries at different
         sites). *)
      let after_of site = List.assoc_opt site r.after in
      let uniform =
        match r.sites with
        | [] -> None
        | first :: rest -> (
            match after_of first with
            | Some preds
              when List.for_all (fun x -> after_of x = Some preds) rest ->
                Some preds
            | _ -> None)
      in
      Buffer.add_string buf
        (Printf.sprintf "# rule %s  support=%d  language=%s%s\n" r.id r.support
           r.language
           (match uniform with
           | Some preds -> "  after=" ^ String.concat "," preds
           | None -> ""));
      Buffer.add_string buf r.pattern_text;
      if r.sites <> [] then
        begin match sites with
        | `Count ->
            (* Reading mode ([--format text-minimal]): the breadth signal
               without the scroll. Mixed per-site [after=] annotations are
               elided with the file lines; the uniform case still shows in
               the rule header above. *)
            Buffer.add_string buf
              (Printf.sprintf "# sites %s  %d file(s)\n" r.id
                 (List.length r.sites))
        | `Full ->
            Buffer.add_string buf (Printf.sprintf "# sites %s\n" r.id);
            List.iter
              (fun p ->
                let annot =
                  if uniform <> None then ""
                  else
                    match after_of p with
                    | Some preds -> "  after=" ^ String.concat "," preds
                    | None -> ""
                in
                Buffer.add_string buf (p ^ annot ^ "\n"))
              r.sites
        end)
    s.rules;
  List.iteri
    (fun i (res : residual) ->
      if i > 0 || s.rules <> [] then Buffer.add_char buf '\n';
      let unparsed =
        (* A residual in an unreadable region is not a factoring failure —
           no rule can match there. Say so on the residual itself, where the
           question "why isn't this a rule?" actually gets asked. *)
        match List.assoc_opt res.res_file s.unparsed with
        | Some rs -> Printf.sprintf "  unparsed=%s" (format_ranges rs)
        | None -> ""
      in
      (match res.res_rules with
      | [] -> Buffer.add_string buf (Printf.sprintf "# residual%s\n" unparsed)
      | ids ->
          Buffer.add_string buf
            (Printf.sprintf "# residual  rule=%s%s\n" (String.concat "," ids)
               unparsed));
      Buffer.add_string buf res.res_diff)
    s.residuals;
  (* Parse-damage footer: one line, and only when there is damage, so a clean
     corpus's summary is untouched. Deliberately not a per-file listing — the
     residuals that the damage explains already carry [unparsed=], and a
     listing repeats them and then adds every file rules covered anyway (62
     lines on daffodil). The count is what the reader cannot get otherwise;
     [--format json] carries the full per-file ranges. *)
  if s.unparsed <> [] then begin
    let affected =
      List.length
        (List.filter
           (fun (res : residual) -> List.mem_assoc res.res_file s.unparsed)
           s.residuals)
    in
    if Buffer.length buf > 0 then Buffer.add_char buf '\n';
    Buffer.add_string buf
      (Printf.sprintf "# parse-errors  files=%d  residuals-affected=%d\n"
         (List.length s.unparsed) affected)
  end;
  Buffer.contents buf

(* ── JSON rendering ─────────────────────────────────────────────── *)

(* Minimal RFC 8259 string escaping: the two mandatory escapes plus the
   common control characters; remaining control bytes as \u00XX. Source
   text is UTF-8 and JSON strings carry UTF-8 verbatim, so all other
   bytes pass through unchanged. *)
let json_escape s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let format_summary_json (s : summary) : string =
  let str x = "\"" ^ json_escape x ^ "\"" in
  let arr xs = "[" ^ String.concat "," xs ^ "]" in
  let obj fields =
    "{"
    ^ String.concat "," (List.map (fun (k, v) -> str k ^ ":" ^ v) fields)
    ^ "}"
  in
  let site_of (r : rule) p =
    let base = [ ("file", str p) ] in
    match List.assoc_opt p r.after with
    | Some preds -> obj (base @ [ ("after", arr (List.map str preds)) ])
    | None -> obj base
  in
  let rule_of (r : rule) =
    obj
      [
        ("id", str r.id);
        ("support", string_of_int r.support);
        ("language", str r.language);
        ("pattern", str r.pattern_text);
        ("sites", arr (List.map (site_of r) r.sites));
      ]
  in
  let residual_of (res : residual) =
    let base =
      [ ("file", str res.res_file) ]
      @ (match res.res_moved_to with
        | Some p -> [ ("moved_to", str p) ]
        | None -> [])
      @ [
          ("rules", arr (List.map str res.res_rules)); ("diff", str res.res_diff);
        ]
    in
    match List.assoc_opt res.res_file s.unparsed with
    | Some rs ->
        obj
          (base
          @ [
              ( "unparsed",
                arr
                  (List.map
                     (fun (a, b) ->
                       obj
                         [ ("from", string_of_int a); ("to", string_of_int b) ])
                     rs) );
            ])
    | None -> obj base
  in
  let unparsed_of (f, rs) =
    obj
      [
        ("file", str f);
        ( "lines",
          arr
            (List.map
               (fun (a, b) ->
                 obj [ ("from", string_of_int a); ("to", string_of_int b) ])
               rs) );
      ]
  in
  obj
    [
      ("rules", arr (List.map rule_of s.rules));
      ("residuals", arr (List.map residual_of s.residuals));
      ("unparsed", arr (List.map unparsed_of s.unparsed));
    ]
  ^ "\n"

(* ── Filesystem loader ──────────────────────────────────────────── *)

let load_from_dirs ~before_dir ~after_dir ?(include_glob = None)
    ?(exclude_dirs =
      [ "node_modules"; ".git"; "_build"; "target"; "__pycache__" ]) ~language
    () : changeset =
  let pred =
    match include_glob with
    | None -> fun _ -> true
    | Some g -> fun p -> File_scan.glob_match g p
  in
  let before_files = File_scan.walk ~exclude_dirs ~pred before_dir [] in
  let after_files = File_scan.walk ~exclude_dirs ~pred after_dir [] in
  let rel_of root path =
    let rlen = String.length root in
    let rlen =
      if String.length path > rlen && path.[rlen] = '/' then rlen + 1 else rlen
    in
    String.sub path rlen (String.length path - rlen)
  in
  let before_rel = List.map (fun p -> (rel_of before_dir p, p)) before_files in
  let after_rel = List.map (fun p -> (rel_of after_dir p, p)) after_files in
  let after_map = Hashtbl.create 32 in
  List.iter (fun (r, p) -> Hashtbl.replace after_map r p) after_rel;
  let files = ref [] in
  List.iter
    (fun (rel, bpath) ->
      match Hashtbl.find_opt after_map rel with
      | Some apath ->
          let bsrc = In_channel.with_open_bin bpath In_channel.input_all in
          let asrc = In_channel.with_open_bin apath In_channel.input_all in
          Hashtbl.remove after_map rel;
          if bsrc <> asrc then
            files :=
              Modified
                {
                  path = rel;
                  moved_to = None;
                  language;
                  before_source = bsrc;
                  after_source = asrc;
                }
              :: !files
      | None ->
          let bsrc = In_channel.with_open_bin bpath In_channel.input_all in
          files :=
            Deleted { path = rel; language; before_source = bsrc } :: !files)
    before_rel;
  Hashtbl.iter
    (fun rel apath ->
      let asrc = In_channel.with_open_bin apath In_channel.input_all in
      files := Added { path = rel; language; after_source = asrc } :: !files)
    after_map;
  { files = List.sort compare !files }

(* ── Manifest loader ────────────────────────────────────────────── *)

(** Read a change-pair manifest (see scripts/diffract-checkout.sh). Two
    directory trees cannot express "this before-file is that differently-named
    after-file", so a producer that knows the pairing — git rename detection, or
    a person correcting one — states it here instead of us inferring it.

    Records are tab-separated; a leading keyword gives the arity so a reader
    never has to guess how many paths follow:

    {v
    pair<TAB>before/path<TAB>after/path    modified, or renamed if they differ
    add<TAB>after/path
    del<TAB>before/path
    v}

    Paths are logical (as the files are named in the project). Field 1 is read
    from [<manifest dir>/before/], field 2 from [<manifest dir>/after/], so one
    argument locates everything. Blank lines and [#] comments are skipped, and
    trailing fields are ignored so the format can gain columns later.

    Anything unrecognised is an error rather than a skip: a manifest that
    quietly drops records would silently shorten the changeset, which reads as a
    codemod with fewer sites rather than as a broken input. *)
let load_from_pairs ~manifest ?(include_glob = None) ~language () : changeset =
  let dir = Filename.dirname manifest in
  let read_file p = In_channel.with_open_bin p In_channel.input_all in
  let matches p =
    match include_glob with None -> true | Some g -> File_scan.glob_match g p
  in
  let lines =
    String.split_on_char '\n' (read_file manifest)
    |> List.mapi (fun i l -> (i + 1, l))
    |> List.filter (fun (_, l) ->
        let t = String.trim l in
        t <> "" && not (String.length t > 0 && t.[0] = '#'))
  in
  let fail lineno msg =
    failwith (Printf.sprintf "%s:%d: %s" manifest lineno msg)
  in
  let side sub p = Filename.concat (Filename.concat dir sub) p in
  let must_read lineno sub p =
    let full = side sub p in
    if not (Sys.file_exists full) then
      fail lineno (Printf.sprintf "no such file: %s" full)
    else read_file full
  in
  let files =
    List.filter_map
      (fun (lineno, line) ->
        match String.split_on_char '\t' line with
        | "pair" :: bp :: ap :: _ ->
            if not (matches bp || matches ap) then None
            else
              let bsrc = must_read lineno "before" bp in
              let asrc = must_read lineno "after" ap in
              let moved_to = if bp = ap then None else Some ap in
              (* An unchanged file that stayed put is not a change. A file that
                 MOVED is, even with identical bytes — the move itself has to
                 reach the output, or applying the summary would leave the file
                 where it was. *)
              if bsrc = asrc && moved_to = None then None
              else
                Some
                  (Modified
                     {
                       path = bp;
                       moved_to;
                       language;
                       before_source = bsrc;
                       after_source = asrc;
                     })
        | "add" :: ap :: _ ->
            if not (matches ap) then None
            else
              Some
                (Added
                   {
                     path = ap;
                     language;
                     after_source = must_read lineno "after" ap;
                   })
        | "del" :: bp :: _ ->
            if not (matches bp) then None
            else
              Some
                (Deleted
                   {
                     path = bp;
                     language;
                     before_source = must_read lineno "before" bp;
                   })
        | ("pair" | "add" | "del") :: _ ->
            fail lineno "too few tab-separated fields for this record"
        | kw :: _ -> fail lineno (Printf.sprintf "unknown record type %S" kw)
        | [] -> None)
      lines
  in
  { files = List.sort compare files }
