(** Line-based unified diff between two strings.

    A small, matcher-independent text diff: a line-level LCS, then hunks with
    three lines of context. Used by the CLI to render the result of a transform
    as a patch. Lifted out of the matcher so it has no dependency on any
    particular matching engine. *)

type diff_op = DKeep of string | DRemove of string | DAdd of string

let generate_diff ?(context = 3) ?keep_hunk ?before_path ~file_path ~original
    ~transformed () =
  if original = transformed then ""
  else
    let orig_lines = String.split_on_char '\n' original in
    let trans_lines = String.split_on_char '\n' transformed in
    let buf = Buffer.create 1024 in
    (* A file that moved needs both of its names. Emitting git's extended
       header for that case — and ONLY that case, so unmoved output is
       untouched — is what lets [git apply] perform the rename as well as the
       content change; the two sides of a plain [--- a/x] / [+++ b/y] pair
       would otherwise be read as an unrelated delete and create. *)
    let before = match before_path with Some p -> p | None -> file_path in
    if before <> file_path then begin
      Buffer.add_string buf
        (Printf.sprintf "diff --git a/%s b/%s\n" before file_path);
      Buffer.add_string buf (Printf.sprintf "rename from %s\n" before);
      Buffer.add_string buf (Printf.sprintf "rename to %s\n" file_path)
    end;
    Buffer.add_string buf (Printf.sprintf "--- a/%s\n" before);
    Buffer.add_string buf (Printf.sprintf "+++ b/%s\n" file_path);
    let orig_arr = Array.of_list orig_lines in
    let trans_arr = Array.of_list trans_lines in
    let ops =
      List.map
        (function
          | Lcs.Keep l -> DKeep l
          | Lcs.Remove l -> DRemove l
          | Lcs.Add l -> DAdd l)
        (Lcs.ops orig_arr trans_arr)
    in
    let context_lines = context in
    let ops_arr = Array.of_list ops in
    let n_ops = Array.length ops_arr in
    let rec find_hunks start_op =
      if start_op >= n_ops then []
      else
        let rec find_change k =
          if k >= n_ops then None
          else
            match ops_arr.(k) with
            | DKeep _ -> find_change (k + 1)
            | _ -> Some k
        in
        match find_change start_op with
        | None -> []
        | Some change_idx ->
            let hunk_start = max start_op (change_idx - context_lines) in
            let rec find_hunk_end k last_change =
              if k >= n_ops then n_ops
              else
                match ops_arr.(k) with
                | DKeep _ ->
                    if k - last_change > 2 * context_lines then k
                    else find_hunk_end (k + 1) last_change
                | _ -> find_hunk_end (k + 1) k
            in
            let hunk_end = find_hunk_end change_idx change_idx in
            let hunk = Array.sub ops_arr hunk_start (hunk_end - hunk_start) in
            (hunk_start, hunk) :: find_hunks hunk_end
    in
    let hunks = find_hunks 0 in
    (* Original-side (0-based) line index at which each op sits: DKeep and
       DRemove advance the original; DAdd does not. Used to report each
       hunk's original position to [keep_hunk]. *)
    let orig_line_at = Array.make (n_ops + 1) 0 in
    let trans_line_at = Array.make (n_ops + 1) 0 in
    for k = 0 to n_ops - 1 do
      orig_line_at.(k + 1) <-
        (orig_line_at.(k)
        + match ops_arr.(k) with DKeep _ | DRemove _ -> 1 | DAdd _ -> 0);
      trans_line_at.(k + 1) <-
        (trans_line_at.(k)
        + match ops_arr.(k) with DKeep _ | DAdd _ -> 1 | DRemove _ -> 0)
    done;
    let emitted = ref false in
    List.iter
      (fun (start_op, hunk) ->
        let orig_start = orig_line_at.(start_op) in
        let trans_start = trans_line_at.(start_op) in
        let orig_len =
          Array.fold_left
            (fun a op ->
              match op with DKeep _ | DRemove _ -> a + 1 | DAdd _ -> a)
            0 hunk
        in
        let trans_len =
          Array.fold_left
            (fun a op ->
              match op with DKeep _ | DAdd _ -> a + 1 | DRemove _ -> a)
            0 hunk
        in
        let keep =
          match keep_hunk with
          | None -> true
          | Some f -> f ~orig_start ~orig_len
        in
        if keep then begin
          emitted := true;
          (* Standard unified-diff hunk header with real line numbers. A
             zero-length side uses the position itself (git's [-0,0] / [-l,0]
             convention); otherwise 1-based. Exact enough for [git apply] with
             the surrounding context lines. *)
          let l1 = if orig_len = 0 then orig_start else orig_start + 1 in
          let l2 = if trans_len = 0 then trans_start else trans_start + 1 in
          Buffer.add_string buf
            (Printf.sprintf "@@ -%d,%d +%d,%d @@\n" l1 orig_len l2 trans_len);
          Array.iter
            (fun op ->
              match op with
              | DKeep s ->
                  Buffer.add_char buf ' ';
                  Buffer.add_string buf s;
                  Buffer.add_char buf '\n'
              | DRemove s ->
                  Buffer.add_char buf '-';
                  Buffer.add_string buf s;
                  Buffer.add_char buf '\n'
              | DAdd s ->
                  Buffer.add_char buf '+';
                  Buffer.add_string buf s;
                  Buffer.add_char buf '\n')
            hunk
        end)
      hunks;
    if !emitted then Buffer.contents buf else ""
