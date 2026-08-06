(** Longest-common-subsequence edit script over two arrays.

    The alignment shared by the line-level unified diff ({!Text_diff}) and the
    token-level edit signatures used to group repeated residual edits
    ({!Cs_group}). Generic in the element type: {!Text_diff} aligns lines,
    {!Cs_group} aligns words within a line.

    Not to be confused with {!Leaf_metric}, which also computes an LCS but
    reports only the {e distance} (via Myers' O(ND), cheap for near-identical
    streams). This one reconstructs the {e script}, so it pays the full
    O(n·m) dynamic-programming table. Callers align short sequences —
    a file's lines, a line's words — never whole corpora. *)

type 'a op = Keep of 'a | Remove of 'a | Add of 'a

let ops (a : 'a array) (b : 'a array) : 'a op list =
  let n = Array.length a in
  let m = Array.length b in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      if a.(i) = b.(j) then dp.(i).(j) <- dp.(i + 1).(j + 1) + 1
      else dp.(i).(j) <- max dp.(i + 1).(j) dp.(i).(j + 1)
    done
  done;
  let out = ref [] in
  let i = ref 0 in
  let j = ref 0 in
  while !i < n || !j < m do
    if !i < n && !j < m && a.(!i) = b.(!j) then begin
      out := Keep a.(!i) :: !out;
      incr i;
      incr j
    end
    else if !i < n && (!j >= m || dp.(!i + 1).(!j) >= dp.(!i).(!j + 1)) then begin
      out := Remove a.(!i) :: !out;
      incr i
    end
    else begin
      out := Add b.(!j) :: !out;
      incr j
    end
  done;
  List.rev !out
