(** Pattern matching DSL for tree-sitter nodes *)

(** A pattern that can match against nodes *)
type t =
  | Any
  (** Matches any node *)
  | Node of string * constraint_ list
  (** Match a node of specific type with constraints *)
  | AnyNode of constraint_ list
  (** Match any node type with constraints *)
  | Or of t list
  (** Match any of the patterns *)
  | Not of t
  (** Match if pattern doesn't match *)

and constraint_ =
  | Field of string * t
  (** Child at field must match pattern *)
  | HasField of string
  (** Must have a child at field (any value) *)
  | Child of t
  (** At least one child matches pattern *)
  | AllChildren of t
  (** All children match pattern *)
  | Text of string
  (** Node text equals string *)
  | TextMatch of (string -> bool)
  (** Node text matches predicate *)
  | Capture of string
  (** Capture this node with given name *)
  | Descendant of t
  (** Some descendant matches pattern *)

(** Match result with captured nodes *)
type match_result = {
  node: Tree.t;
  captures: (string * Tree.t) list;
}

(** {1 Pattern constructors} *)

let any = Any

let node type_name constraints = Node (type_name, constraints)

let any_node constraints = AnyNode constraints

let ( ||| ) p1 p2 = match p1, p2 with
  | Or ps1, Or ps2 -> Or (ps1 @ ps2)
  | Or ps, p | p, Or ps -> Or (p :: ps)
  | _ -> Or [p1; p2]

let not_ p = Not p

(** {1 Constraint constructors} *)

let field name pattern = Field (name, pattern)

let has_field name = HasField name

let child pattern = Child pattern

let all_children pattern = AllChildren pattern

let has_text s = Text s

let text_matches f = TextMatch f

let capture name = Capture name

let descendant pattern = Descendant pattern

(** {1 Matching} *)

let rec match_pattern source pattern node =
  match pattern with
  | Any -> Some { node; captures = [] }

  | Node (type_name, constraints) ->
    if Tree.node_type node = type_name then
      match_constraints source constraints node
    else
      None

  | AnyNode constraints ->
    if Tree.is_named node then
      match_constraints source constraints node
    else
      None

  | Or patterns ->
    List.find_map (fun p -> match_pattern source p node) patterns

  | Not pattern ->
    (match match_pattern source pattern node with
     | Some _ -> None
     | None -> Some { node; captures = [] })

and match_constraints source constraints node =
  let rec go captures = function
    | [] -> Some { node; captures }
    | c :: rest ->
      match match_constraint source c node captures with
      | None -> None
      | Some new_captures -> go new_captures rest
  in
  go [] constraints

and match_constraint source constraint_ node captures =
  match constraint_ with
  | Field (name, pattern) ->
    (match Tree.field node name with
     | None -> None
     | Some child ->
       match match_pattern source pattern child with
       | None -> None
       | Some result -> Some (result.captures @ captures))

  | HasField name ->
    (match Tree.field node name with
     | None -> None
     | Some _ -> Some captures)

  | Child pattern ->
    let children = Tree.named_children node in
    (match List.find_map (match_pattern source pattern) children with
     | None -> None
     | Some result -> Some (result.captures @ captures))

  | AllChildren pattern ->
    let children = Tree.named_children node in
    let results = List.filter_map (match_pattern source pattern) children in
    if List.length results = List.length children then
      let all_captures = List.concat_map (fun r -> r.captures) results in
      Some (all_captures @ captures)
    else
      None

  | Text expected ->
    if Tree.text source node = expected then Some captures
    else None

  | TextMatch pred ->
    if pred (Tree.text source node) then Some captures
    else None

  | Capture name ->
    Some ((name, node) :: captures)

  | Descendant pattern ->
    let found = ref None in
    (try
       Tree.traverse (fun n ->
         match match_pattern source pattern n with
         | Some result ->
           found := Some result;
           raise Exit
         | None -> ()
       ) node
     with Exit -> ());
    (match !found with
     | None -> None
     | Some result -> Some (result.captures @ captures))

(** Try to match a pattern against a node *)
let matches source pattern node =
  match match_pattern source pattern node with
  | Some _ -> true
  | None -> false

(** Match and return result with captures *)
let match_node source pattern node =
  match_pattern source pattern node

(** Find all nodes in tree matching the pattern *)
let find_all source pattern root =
  let results = ref [] in
  Tree.traverse (fun node ->
    match match_pattern source pattern node with
    | Some result -> results := result :: !results
    | None -> ()
  ) root;
  List.rev !results

(** Get a captured node by name from a match result *)
let get_capture name result =
  List.assoc_opt name result.captures

(** Get all captured nodes by name (for repeated captures) *)
let get_captures name result =
  List.filter_map (fun (n, node) ->
    if n = name then Some node else None
  ) result.captures

(** {1 Convenience patterns} *)

(** Match a node and capture it *)
let capture_node name type_name =
  node type_name [capture name]

(** Match any named node and capture it *)
let capture_any name =
  any_node [capture name]

(** Match a node with specific text *)
let node_with_text type_name text =
  node type_name [has_text text]

(** Match identifier with specific name *)
let identifier name =
  node "identifier" [has_text name]

(** Match any identifier and capture it *)
let any_identifier cap_name =
  node "identifier" [capture cap_name]
