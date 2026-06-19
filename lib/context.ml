(* A bounded two-generation (~LRU) memo of parsed trees, keyed on
   (language, source).

   The stored value is a [unit Tree_types.tree] — the bare record type lives in
   {!Tree_types} precisely so [Context] (which sits below [Tree]) can name it
   without a dependency cycle. Trees are pure, immutable OCaml data, so handing
   one cached parse to many readers is safe.

   Two generations give O(1) eviction that keeps frequently-reused inputs (a
   changeset's source files, re-parsed once per candidate by the change-summary
   gate) hot, while one-shot parses (transformed intermediates, pattern bodies)
   age out: a lookup that hits [prev] is promoted to [cur], so anything touched
   within the last rotation survives the next one. *)
type parse_memo = {
  mutable cur : (string * string, unit Tree_types.tree) Hashtbl.t;
  mutable prev : (string * string, unit Tree_types.tree) Hashtbl.t;
  mutable cap : int;
}

type t = {
  lang_cache : (string, nativeint) Hashtbl.t;
  parse_memo : parse_memo;
}

let create ?(parse_cache_cap = 512) () =
  {
    lang_cache = Hashtbl.create 16;
    parse_memo =
      {
        cur = Hashtbl.create (parse_cache_cap * 2);
        prev = Hashtbl.create 16;
        cap = parse_cache_cap;
      };
  }

let parse_memo_find ctx key =
  let m = ctx.parse_memo in
  match Hashtbl.find_opt m.cur key with
  | Some _ as r -> r
  | None -> (
      match Hashtbl.find_opt m.prev key with
      | Some v ->
          (* promote so a still-live input survives the next rotation *)
          Hashtbl.replace m.cur key v;
          Some v
      | None -> None)

let parse_memo_add ctx key v =
  let m = ctx.parse_memo in
  if Hashtbl.length m.cur >= m.cap then begin
    m.prev <- m.cur;
    m.cur <- Hashtbl.create (m.cap * 2)
  end;
  Hashtbl.replace m.cur key v

(* Grow the parse cache so a known working set fits without thrashing. A caller
   that will repeatedly re-parse a fixed set of inputs (the change-summary gate
   re-evaluating every candidate against the changeset's files) calls this once
   the set's size is known, so the cap never falls below it however large the
   changeset. Only ever raises the cap. *)
let ensure_parse_cap ctx n = if n > ctx.parse_memo.cap then ctx.parse_memo.cap <- n
