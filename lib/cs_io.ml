(** Change-summary I/O: render a {!Cs_types.summary} to the [.summary] text
    format ([format_summary]) and load a {!Cs_types.changeset} by pairing two
    directory trees ([load_from_dirs]). Depends only on {!Cs_types}. *)

open Cs_types

let format_summary (s : summary) : string =
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
      if r.sites <> [] then begin
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
