(** Dump every child including unnamed tokens. Used to diagnose what the grammar
    actually exposes as nodes versus what is consumed silently. *)

let rec dump indent (node : Diffract.Tree.src Diffract.Tree.t) source =
  let pad = String.make indent ' ' in
  let named = if node.is_named then "named" else "unnamed" in
  let text =
    if node.children = [] then
      let t = Diffract.Tree.text source node in
      Printf.sprintf " %S" t
    else ""
  in
  Printf.printf "%s%s [%s]%s\n" pad node.node_type named text;
  List.iter
    (fun (c : _ Diffract.Tree.child) -> dump (indent + 2) c.node source)
    node.children

let () =
  let src = Sys.argv.(1) in
  let lang = Sys.argv.(2) in
  let ctx = Diffract.Context.create () in
  let t = Diffract.Tree.parse ~ctx ~language:lang src in
  dump 0 t.root src
