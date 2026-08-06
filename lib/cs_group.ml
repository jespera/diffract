(** Grouping of repeated residual edits — the reading-mode digest.

    Residuals are where a summary's bulk lives, and they repeat: the same
    one-line rename in fifty files, the same import inserted in thirty-nine.
    Rendered one file at a time, that repetition is invisible and the reader
    scrolls. This module groups it.

    Deliberately a {b rendering} pass over {!Cs_types.residual} values that are
    already final, not a pipeline phase: it re-parses each residual's rendered
    diff rather than consulting the tree. So it cannot affect rule derivation,
    costs no gate evaluation or reparse, and leaves the canonical [text] format
    byte-identical — only [text-minimal] consumes it.

    A consequence worth stating: because it works on residual text, it reaches
    changes the matcher cannot. Comments are tree-sitter extras that no rule can
    ever match, and pure insertions have no anchor to hang a pattern on; both
    group here like anything else. The digest makes such changes {e legible}. It
    does not make them {e applicable} — nothing here becomes a rule. *)

open Cs_types

(* ── word tokenization ───────────────────────────────────────────── *)

(* Bytes ≥ 128 count as word characters so a UTF-8 sequence is never split
   mid-character. *)
let is_word_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'
  || Char.code c >= 128

let is_blank c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

let is_upper c = c >= 'A' && c <= 'Z'
let is_lower c = c >= 'a' && c <= 'z'

(** Split one word-character run at case humps and underscores:
    [AcmeWidgetLibraryService] into [Acme|Widget|Library|Service],
    [HTTPServer] into [HTTP|Server], [snake_case] into [snake|_|case].

    Without this the {e conceptual} edit "delete the word Library" reads as a
    different literal edit in every identifier it touches
    ([AcmeWidgetLibraryService ⤳ AcmeWidgetService] versus
    [AcmeReportLibraryModule ⤳ AcmeReportModule]), so a systematic rename
    fragments into one group per name. Measured on a rename corpus, hump
    splitting takes 11 renamed paths from 9 distinct edits to 3; measured on
    every public corpus in [evaluation/], it changes grouping not at all. Bytes
    ≥ 128 are neither upper nor lower here, so UTF-8 is never split. *)
let split_humps (w : string) : string list =
  let n = String.length w in
  let out = ref [] and start = ref 0 in
  let cut i =
    if i > !start then out := String.sub w !start (i - !start) :: !out;
    start := i
  in
  for i = 1 to n - 1 do
    let prev = w.[i - 1] and cur = w.[i] in
    if cur = '_' || prev = '_' then cut i
    else if is_upper cur && not (is_upper prev) then cut i
    else if
      (* the tail of an acronym that runs into a word: HTTP|Server *)
      is_upper prev && is_upper cur && i + 1 < n && is_lower w.[i + 1]
    then cut i
  done;
  cut n;
  List.rev !out

(** Split into word pieces, runs of whitespace, and single punctuation bytes —
    the granularity at which two lines "differ by the same edit". Coarser than
    characters (so [akka] is one unit, not four) and finer than
    whitespace-separated tokens (so [akka.cluster.X] splits at the dots, and
    {!split_humps} splits inside each piece). *)
let words (s : string) : string array =
  let n = String.length s in
  let out = ref [] in
  let i = ref 0 in
  while !i < n do
    let start = !i in
    if is_word_char s.[!i] then begin
      while !i < n && is_word_char s.[!i] do incr i done;
      out := List.rev_append (split_humps (String.sub s start (!i - start))) !out
    end
    else begin
      if is_blank s.[!i] then while !i < n && is_blank s.[!i] do incr i done
      else incr i;
      out := String.sub s start (!i - start) :: !out
    end
  done;
  Array.of_list (List.rev !out)

(* ── edit signatures ─────────────────────────────────────────────── *)

(** The differing runs of an alignment, as [(removed, added)] text pairs — what
    makes two hunks "the same edit" regardless of the code around them.

    Runs that are whitespace on both sides are dropped: a reflowed line that
    also gains an import should group with the plain import insertion, not
    split off into a group of its own. *)
let signature (before : string) (after : string) : (string * string) list =
  let rm = Buffer.create 32 and ad = Buffer.create 32 in
  let keep = Buffer.create 16 and keep_n = ref 0 in
  let segs = ref [] in
  let pending () = Buffer.length rm > 0 || Buffer.length ad > 0 in
  let flush () =
    let r = String.trim (Buffer.contents rm) in
    let a = String.trim (Buffer.contents ad) in
    Buffer.clear rm;
    Buffer.clear ad;
    if not (r = "" && a = "") then segs := (r, a) :: !segs
  in
  (* A lone punctuation token between two changed runs is a separator, not
     context: renaming [akka/] to [org/apache/pekko/] keeps one [/] in the
     middle, which would otherwise split the rename into "akka -> org" plus
     "insert apache/pekko/" — two segments describing one edit. Absorb such a
     separator into both sides instead of ending the segment. A kept *word*
     ends it, as does a run of more than one token. *)
  let separator () =
    !keep_n = 1
    &&
    let k = Buffer.contents keep in
    String.length k = 1 && (not (is_word_char k.[0])) && not (is_blank k.[0])
  in
  (* Only a kept run ends (or bridges) a segment; consecutive removes and adds
     accumulate into the same one. *)
  let settle () =
    if !keep_n > 0 then begin
      if pending () then
        if separator () then begin
          Buffer.add_string rm (Buffer.contents keep);
          Buffer.add_string ad (Buffer.contents keep)
        end
        else flush ();
      Buffer.clear keep;
      keep_n := 0
    end
  in
  List.iter
    (function
      | Lcs.Keep w ->
          Buffer.add_string keep w;
          incr keep_n
      | Lcs.Remove w ->
          settle ();
          Buffer.add_string rm w
      | Lcs.Add w ->
          settle ();
          Buffer.add_string ad w)
    (Lcs.ops (words before) (words after));
  flush ();
  (* Collapse repeats: a hunk that makes the same edit twice describes the same
     change as one that makes it once, and keeping the multiplicity splits it
     into a group of its own ("akka. -> org.apache.pekko." separate from
     "akka. -> org.apache.pekko.; akka. -> org.apache.pekko."). First-occurrence
     order is kept, so a hunk doing A then B still differs from one doing only
     A. Measured: pekko 5 groups to 3, every other corpus unchanged. *)
  let seen = Hashtbl.create 8 in
  List.rev !segs
  |> List.filter (fun s ->
         if Hashtbl.mem seen s then false
         else begin
           Hashtbl.add seen s ();
           true
         end)

(* Truncate to at most [n] bytes without splitting a UTF-8 sequence. *)
let ellipsize n s =
  if String.length s <= n then s
  else begin
    let cut = ref n in
    while !cut > 0 && Char.code s.[!cut] land 0xC0 = 0x80 do decr cut done;
    String.sub s 0 !cut ^ "…"
  end

(** One-line rendering of a signature. The exemplar printed beneath a group
    carries the context, so this stays terse. *)
let describe (segs : (string * string) list) : string =
  let one (r, a) =
    let r = ellipsize 40 (ws_collapse r) and a = ellipsize 40 (ws_collapse a) in
    if r = "" then "(insert) " ^ a
    else if a = "" then "(delete) " ^ r
    else r ^ " -> " ^ a
  in
  match segs with
  | [] -> "(no textual change)"
  | segs -> String.concat "; " (List.map one segs)

(* ── parsing a rendered residual diff ────────────────────────────── *)

type hunk = {
  hk_minus : string list;  (** removed lines, sigil stripped *)
  hk_plus : string list;  (** added lines, sigil stripped *)
  hk_body : string;  (** the hunk verbatim, header included, for re-emission *)
}

type parsed = {
  pd_hunks : hunk list;
  pd_from : string option;
  pd_to : string option;
  pd_rename_only : bool;
}

let starts_with p s =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let after_prefix p s =
  String.sub s (String.length p) (String.length s - String.length p)

let parse_diff (d : string) : parsed =
  let lines = String.split_on_char '\n' d in
  (* [res_diff] ends in a newline; drop the empty tail so it is not read as a
     context line of the final hunk. *)
  let lines =
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
  in
  let hunks = ref [] and head = ref [] and cur = ref None in
  let close () =
    match !cur with
    | None -> ()
    | Some (minus, plus, body) ->
        hunks :=
          {
            hk_minus = List.rev minus;
            hk_plus = List.rev plus;
            hk_body = String.concat "\n" (List.rev body);
          }
          :: !hunks;
        cur := None
  in
  List.iter
    (fun l ->
      if starts_with "@@" l then begin
        close ();
        cur := Some ([], [], [ l ])
      end
      else
        match !cur with
        | None -> head := l :: !head
        | Some (m, p, b) ->
            let m, p =
              if starts_with "-" l then (after_prefix "-" l :: m, p)
              else if starts_with "+" l then (m, after_prefix "+" l :: p)
              else (m, p)
            in
            cur := Some (m, p, l :: b))
    lines;
  close ();
  let head = List.rev !head in
  let find p =
    List.find_map (fun l -> if starts_with p l then Some (after_prefix p l) else None) head
  in
  let hunks = List.rev !hunks in
  {
    pd_hunks = hunks;
    pd_from = find "rename from ";
    pd_to = find "rename to ";
    pd_rename_only =
      hunks = [] && List.exists (starts_with "similarity index 100%") head;
  }

(* ── the digest ──────────────────────────────────────────────────── *)

type group = {
  g_edit : string;
  g_count : int;
  g_files : int;
  g_exemplar : string list * string list;
}

type rename_edit = {
  re_edit : string;
  re_count : int;
  re_exemplar : string * string;
}

type digest = {
  dg_renames : (int * rename_edit list) option;
      (** [(file count, the distinct path edits)] — [None] when nothing moved *)
  dg_groups : group list;  (** repeated edits, most frequent first *)
  dg_grouped : int;  (** hunks a group accounts for *)
  dg_total : int;  (** hunks in all content residuals *)
  dg_rest : (residual * string list) list;
      (** residuals with their grouped hunks removed, as surviving hunk bodies;
          residuals left with nothing are dropped *)
}

let key_of segs = String.concat "\x00" (List.concat_map (fun (r, a) -> [ r; a ]) segs)

let digest (residuals : residual list) : digest =
  let parsed = List.map (fun r -> (r, parse_diff r.res_diff)) residuals in
  let renamed, content =
    List.partition (fun (_, p) -> p.pd_rename_only) parsed
  in
  (* Path edits: the same signature machinery, applied to the two names of a
     moved file rather than the two sides of a hunk. A systematic move ("every
     file under akka/ went to org/apache/pekko/") collapses to one line.

     Over EVERY moved file, not just the rename-only ones — a file that both
     moved and kept some unexplained content is still a move, and counting only
     the content-free ones would report 86 of pekko's 131 renames. Taken from
     [res_moved_to] rather than the rendered headers, since that is where the
     fact lives. *)
  let moved =
    List.filter
      (fun ((r : residual), _) ->
        match r.res_moved_to with Some t -> t <> r.res_file | None -> false)
      parsed
  in
  let dg_renames =
    if moved = [] then None
    else begin
      let tbl = Hashtbl.create 8 and order = ref [] in
      List.iter
        (fun ((r : residual), _) ->
          let f = r.res_file in
          let t = Option.value r.res_moved_to ~default:r.res_file in
          let segs = signature f t in
          let k = key_of segs in
          match Hashtbl.find_opt tbl k with
          | Some cell -> incr (fst cell)
          | None ->
              Hashtbl.add tbl k (ref 1, (describe segs, (f, t)));
              order := k :: !order)
        moved;
      let edits =
        List.rev !order
        |> List.map (fun k ->
               let n, (edit, ex) = Hashtbl.find tbl k in
               { re_edit = edit; re_count = !n; re_exemplar = ex })
        |> List.sort (fun a b ->
               match compare b.re_count a.re_count with
               | 0 -> compare a.re_edit b.re_edit
               | c -> c)
      in
      Some (List.length moved, edits)
    end
  in
  (* Hunk groups. Counting and exemplar selection both walk residuals in list
     order and hunks in file order, so the chosen exemplar is deterministic —
     a wobbling exemplar would churn every baseline on every run. *)
  let tbl = Hashtbl.create 64 and order = ref [] in
  let total = ref 0 in
  List.iter
    (fun ((r : residual), p) ->
      List.iter
        (fun h ->
          incr total;
          let segs =
            signature
              (String.concat "\n" h.hk_minus)
              (String.concat "\n" h.hk_plus)
          in
          let k = key_of segs in
          match Hashtbl.find_opt tbl k with
          | Some (n, files, _) ->
              incr n;
              if not (List.mem r.res_file !files) then files := r.res_file :: !files
          | None ->
              Hashtbl.add tbl k
                (ref 1, ref [ r.res_file ], (describe segs, (h.hk_minus, h.hk_plus)));
              order := k :: !order)
        p.pd_hunks)
    content;
  let grouped_keys = Hashtbl.create 16 in
  let dg_groups =
    List.rev !order
    |> List.filter_map (fun k ->
           let n, files, (edit, ex) = Hashtbl.find tbl k in
           if !n < 2 then None
           else begin
             Hashtbl.replace grouped_keys k ();
             Some
               {
                 g_edit = edit;
                 g_count = !n;
                 g_files = List.length !files;
                 g_exemplar = ex;
               }
           end)
    |> List.sort (fun a b ->
           match compare b.g_count a.g_count with
           | 0 -> compare a.g_edit b.g_edit
           | c -> c)
  in
  let dg_grouped =
    List.fold_left (fun acc g -> acc + g.g_count) 0 dg_groups
  in
  let dg_rest =
    List.filter_map
      (fun ((r : residual), p) ->
        let keep =
          List.filter
            (fun h ->
              let k =
                key_of
                  (signature
                     (String.concat "\n" h.hk_minus)
                     (String.concat "\n" h.hk_plus))
              in
              not (Hashtbl.mem grouped_keys k))
            p.pd_hunks
        in
        if keep = [] then None
        else Some (r, List.map (fun h -> h.hk_body) keep))
      content
  in
  { dg_renames; dg_groups; dg_grouped; dg_total = !total; dg_rest }
