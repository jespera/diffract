(** Line-based unified diff between two strings.

    A small, matcher-independent text diff: a line-level LCS, then hunks with
    three lines of context. Used by the CLI to render the result of a transform
    as a patch. Lifted out of the matcher so it has no dependency on any
    particular matching engine. *)

type diff_op = DKeep of string | DRemove of string | DAdd of string

let generate_diff ?(context = 3) ?keep_hunk ~file_path ~original ~transformed ()
    =
  if original = transformed then ""
  else
    let orig_lines = String.split_on_char '\n' original in
    let trans_lines = String.split_on_char '\n' transformed in
    let buf = Buffer.create 1024 in
    Buffer.add_string buf (Printf.sprintf "--- a/%s\n" file_path);
    Buffer.add_string buf (Printf.sprintf "+++ b/%s\n" file_path);
    let orig_arr = Array.of_list orig_lines in
    let trans_arr = Array.of_list trans_lines in
    let n = Array.length orig_arr in
    let m = Array.length trans_arr in
    let dp = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = n - 1 downto 0 do
      for j = m - 1 downto 0 do
        if orig_arr.(i) = trans_arr.(j) then
          dp.(i).(j) <- dp.(i + 1).(j + 1) + 1
        else dp.(i).(j) <- max dp.(i + 1).(j) dp.(i).(j + 1)
      done
    done;
    let ops = ref [] in
    let i = ref 0 in
    let j = ref 0 in
    while !i < n || !j < m do
      if !i < n && !j < m && orig_arr.(!i) = trans_arr.(!j) then (
        ops := DKeep orig_arr.(!i) :: !ops;
        incr i;
        incr j)
      else if !i < n && (!j >= m || dp.(!i + 1).(!j) >= dp.(!i).(!j + 1)) then (
        ops := DRemove orig_arr.(!i) :: !ops;
        incr i)
      else (
        ops := DAdd trans_arr.(!j) :: !ops;
        incr j)
    done;
    let ops = List.rev !ops in
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
    for k = 0 to n_ops - 1 do
      orig_line_at.(k + 1) <-
        (orig_line_at.(k)
        + match ops_arr.(k) with DKeep _ | DRemove _ -> 1 | DAdd _ -> 0)
    done;
    let emitted = ref false in
    List.iter
      (fun (start_op, hunk) ->
        let keep =
          match keep_hunk with
          | None -> true
          | Some f ->
              let orig_start = orig_line_at.(start_op) in
              let orig_len =
                Array.fold_left
                  (fun a op ->
                    match op with DKeep _ | DRemove _ -> a + 1 | DAdd _ -> a)
                  0 hunk
              in
              f ~orig_start ~orig_len
        in
        if keep then begin
          emitted := true;
          Buffer.add_string buf "@@ ... @@\n";
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
