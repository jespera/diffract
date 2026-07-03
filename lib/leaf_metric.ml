(* Token-level edit metric over tree-sitter leaf streams. See leaf_metric.mli. *)

type stream = string array

let leaves ~source (root : 'a Tree.t) : stream =
  let acc = ref [] in
  let rec walk (n : 'a Tree.t) =
    match n.Tree.children with
    | [] ->
        (* Skip zero-width "missing" leaves — error-recovery phantoms whose
           empty text is not part of the file's content. *)
        if n.start_byte <> n.end_byte then acc := Tree.text source n :: !acc
    | children -> List.iter (fun (c : 'a Tree.child) -> walk c.node) children
  in
  walk root;
  Array.of_list (List.rev !acc)

(* Intern the given streams into int arrays sharing one token→id table, so
   the Myers inner loop compares ints instead of strings. *)
let intern (streams : stream list) : int array list =
  let tbl = Hashtbl.create 256 in
  let id s =
    match Hashtbl.find_opt tbl s with
    | Some i -> i
    | None ->
        let i = Hashtbl.length tbl in
        Hashtbl.add tbl s i;
        i
  in
  List.map (Array.map id) streams

(* Myers' greedy O(ND) LCS distance (An O(ND) Difference Algorithm, 1986,
   fig. 2 — the distance-only forward pass), cut off at [max_d]: [Some d]
   if the distance is d ≤ max_d, [None] otherwise. The common prefix and
   suffix are trimmed first; the d=0 snake would absorb the prefix anyway,
   but trimming both ends shrinks every later snake. *)
let myers ~max_d (a : int array) (b : int array) : int option =
  if max_d < 0 then None
  else
    let n = Array.length a and m = Array.length b in
    let pre = ref 0 in
    while !pre < n && !pre < m && a.(!pre) = b.(!pre) do incr pre done;
    let suf = ref 0 in
    while
      !suf < n - !pre && !suf < m - !pre && a.(n - 1 - !suf) = b.(m - 1 - !suf)
    do
      incr suf
    done;
    let off = !pre in
    let n = n - !pre - !suf and m = m - !pre - !suf in
    if n = 0 && m = 0 then Some 0
    else begin
      let max_d = min max_d (n + m) in
      (* v.(vi k) = furthest x reached on diagonal k with the current d;
         padded one slot on each side so the k±1 reads at k = ±d stay in
         bounds. *)
      let v = Array.make ((2 * max_d) + 3) 0 in
      let vi k = k + max_d + 1 in
      let exception Found of int in
      try
        for d = 0 to max_d do
          let k = ref (-d) in
          while !k <= d do
            let x =
              if !k = -d || (!k <> d && v.(vi (!k - 1)) < v.(vi (!k + 1))) then
                v.(vi (!k + 1))
              else v.(vi (!k - 1)) + 1
            in
            let x = ref x in
            let y = ref (!x - !k) in
            while !x < n && !y < m && a.(off + !x) = b.(off + !y) do
              incr x;
              incr y
            done;
            v.(vi !k) <- !x;
            if !x >= n && !y >= m then raise (Found d);
            k := !k + 2
          done
        done;
        None
      with Found d -> Some d
    end

let distance a b =
  match intern [ a; b ] with
  | [ ia; ib ] ->
      Option.get (myers ~max_d:(Array.length a + Array.length b) ia ib)
  | _ -> assert false

let distance_upto ~bound a b =
  match intern [ a; b ] with
  | [ ia; ib ] -> myers ~max_d:bound ia ib
  | _ -> assert false

let geodesic ?d_endpoints ~before ~mid ~after () =
  match intern [ before; mid; after ] with
  | [ ib; im; ia ] ->
      let d_ba =
        match d_endpoints with
        | Some d -> d
        | None ->
            Option.get
              (myers ~max_d:(Array.length before + Array.length after) ib ia)
      in
      (match myers ~max_d:d_ba ib im with
      | None -> false
      | Some d1 ->
          (* Triangle inequality: d(mid,after) ≥ d_ba − d1, so a search cut
             off at d_ba − d1 succeeds exactly when equality holds. *)
          myers ~max_d:(d_ba - d1) im ia <> None)
  | _ -> assert false
