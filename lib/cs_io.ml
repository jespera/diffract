(** Change-summary I/O: render a {!Cs_types.summary} to the [.summary] text
    format ([format_summary]) and load a {!Cs_types.changeset} by pairing two
    directory trees ([load_from_dirs]). Depends only on {!Cs_types}. *)

open Cs_types

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
      (match res.res_rules with
      | [] -> Buffer.add_string buf "# residual\n"
      | ids ->
          Buffer.add_string buf
            (Printf.sprintf "# residual  rule=%s\n" (String.concat "," ids)));
      Buffer.add_string buf res.res_diff)
    s.residuals;
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
    obj
      [
        ("file", str res.res_file);
        ("rules", arr (List.map str res.res_rules));
        ("diff", str res.res_diff);
      ]
  in
  obj
    [
      ("rules", arr (List.map rule_of s.rules));
      ("residuals", arr (List.map residual_of s.residuals));
    ]
  ^ "\n"

(* ── Filesystem loader ──────────────────────────────────────────── *)

let default_ext_language = [ (".tsx", "tsx"); (".ts", "typescript") ]

let language_of_file path ~default ~ext_language =
  match
    List.find_opt (fun (ext, _) -> Filename.check_suffix path ext) ext_language
  with
  | Some (_, lang) -> lang
  | None -> default

let load_from_dirs ~before_dir ~after_dir ?(include_glob = None)
    ?(exclude_dirs =
      [ "node_modules"; ".git"; "_build"; "target"; "__pycache__" ])
    ?(ext_language = default_ext_language) ~default_language () : changeset =
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
      let lang = language_of_file rel ~default:default_language ~ext_language in
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
                  language = lang;
                  before_source = bsrc;
                  after_source = asrc;
                }
              :: !files
      | None ->
          let bsrc = In_channel.with_open_bin bpath In_channel.input_all in
          files :=
            Deleted { path = rel; language = lang; before_source = bsrc }
            :: !files)
    before_rel;
  Hashtbl.iter
    (fun rel apath ->
      let lang = language_of_file rel ~default:default_language ~ext_language in
      let asrc = In_channel.with_open_bin apath In_channel.input_all in
      files :=
        Added { path = rel; language = lang; after_source = asrc } :: !files)
    after_map;
  { files = List.sort compare !files }
