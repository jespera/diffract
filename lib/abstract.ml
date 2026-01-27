(** Change abstraction - Replace concrete values with placeholders *)

(** Types of placeholders used in abstraction *)
type placeholder_kind =
  | Identifier   (** Named identifiers: variable names, function names, etc. *)
  | StringLit    (** String literals *)
  | NumberLit    (** Number literals *)
  | TypeName     (** Type identifiers *)
  | Other        (** Other leaf nodes *)

(** A placeholder in an abstracted template *)
type placeholder = {
  kind: placeholder_kind;
  index: int;  (** Unique index for this placeholder within its kind *)
  original_text: string;  (** The original concrete value *)
}

(** An abstracted node - either structural or a placeholder *)
type abstract_node =
  | Structural of {
      node_type: string;
      children: abstract_node list;
    }
  | Placeholder of placeholder

(** An abstracted change template *)
type abstract_change = {
  change_kind: [ `Added | `Removed | `Modified | `Replaced ];
  context: Diff.context;
  before: abstract_node option;
  after: abstract_node option;
}

(** State for abstraction - tracks which values map to which placeholders *)
type abstraction_state = {
  mutable id_counter: int;
  mutable str_counter: int;
  mutable num_counter: int;
  mutable type_counter: int;
  mutable other_counter: int;
  value_map: (string, placeholder) Hashtbl.t;
}

let create_state () = {
  id_counter = 0;
  str_counter = 0;
  num_counter = 0;
  type_counter = 0;
  other_counter = 0;
  value_map = Hashtbl.create 16;
}

(** Node types that represent identifiers *)
let identifier_types = [
  "identifier";
  "property_identifier";
  "shorthand_property_identifier";
  "shorthand_property_identifier_pattern";
]

(** Node types that represent type names *)
let type_identifier_types = [
  "type_identifier";
  "predefined_type";
]

(** Node types that represent literals *)
let string_literal_types = [
  "string";
  "string_fragment";
  "template_string";
]

let number_literal_types = [
  "number";
]

(** Determine the placeholder kind for a node type *)
let placeholder_kind_of_node_type node_type =
  if List.mem node_type identifier_types then Some Identifier
  else if List.mem node_type type_identifier_types then Some TypeName
  else if List.mem node_type string_literal_types then Some StringLit
  else if List.mem node_type number_literal_types then Some NumberLit
  else None

(** Get or create a placeholder for a value *)
let get_or_create_placeholder state kind text =
  let key = Printf.sprintf "%s:%s"
    (match kind with
     | Identifier -> "id"
     | StringLit -> "str"
     | NumberLit -> "num"
     | TypeName -> "type"
     | Other -> "other")
    text
  in
  match Hashtbl.find_opt state.value_map key with
  | Some p -> p
  | None ->
    let index = match kind with
      | Identifier ->
        let i = state.id_counter in
        state.id_counter <- i + 1; i
      | StringLit ->
        let i = state.str_counter in
        state.str_counter <- i + 1; i
      | NumberLit ->
        let i = state.num_counter in
        state.num_counter <- i + 1; i
      | TypeName ->
        let i = state.type_counter in
        state.type_counter <- i + 1; i
      | Other ->
        let i = state.other_counter in
        state.other_counter <- i + 1; i
    in
    let p = { kind; index; original_text = text } in
    Hashtbl.add state.value_map key p;
    p

(** Abstract a single node *)
let rec abstract_node state source node =
  let node_type = Tree.node_type node in
  let children = Tree.named_children node in

  if children = [] then
    (* Leaf node - potentially a placeholder *)
    let text = Tree.text source node in
    match placeholder_kind_of_node_type node_type with
    | Some kind ->
      let p = get_or_create_placeholder state kind text in
      Placeholder p
    | None ->
      (* Keep as structural with the text embedded in type *)
      Structural { node_type; children = [] }
  else
    (* Non-leaf: recurse into children *)
    let abstract_children = List.map (abstract_node state source) children in
    Structural { node_type; children = abstract_children }

(** Abstract a diff change *)
let abstract_change diff_result change =
  let state = create_state () in
  match change with
  | Diff.Added { context; node; _ } ->
    let after_abs = abstract_node state diff_result.Diff.after_source node in
    {
      change_kind = `Added;
      context;
      before = None;
      after = Some after_abs;
    }
  | Diff.Removed { context; node; _ } ->
    let before_abs = abstract_node state diff_result.Diff.before_source node in
    {
      change_kind = `Removed;
      context;
      before = Some before_abs;
      after = None;
    }
  | Diff.Modified { context; before; after; _ } ->
    let before_abs = abstract_node state diff_result.Diff.before_source before in
    let after_abs = abstract_node state diff_result.Diff.after_source after in
    {
      change_kind = `Modified;
      context;
      before = Some before_abs;
      after = Some after_abs;
    }
  | Diff.Replaced { context; before; after; _ } ->
    let before_abs = abstract_node state diff_result.Diff.before_source before in
    let after_abs = abstract_node state diff_result.Diff.after_source after in
    {
      change_kind = `Replaced;
      context;
      before = Some before_abs;
      after = Some after_abs;
    }

(** {1 Template representation (for comparison)} *)

(** A normalized template that can be compared across changes *)
type template = {
  structure: abstract_node option * abstract_node option;
  (** The abstract before/after pair with normalized placeholder indices *)
}

(** Renumber placeholders consistently for comparison *)
let normalize_node node =
  let state = create_state () in
  let rec renumber = function
    | Structural { node_type; children } ->
      Structural { node_type; children = List.map renumber children }
    | Placeholder p ->
      (* Create fresh placeholder with same kind but new index *)
      let new_p = get_or_create_placeholder state p.kind p.original_text in
      (* But we want same values to get same indices, ignoring original text *)
      Placeholder { new_p with original_text = "" }
  in
  renumber node

(** Create a comparable template from an abstract change *)
let to_template change =
  {
    structure = (
      Option.map normalize_node change.before,
      Option.map normalize_node change.after
    );
  }

(** Compare two templates for structural equality *)
let rec nodes_structurally_equal n1 n2 =
  match n1, n2 with
  | Structural s1, Structural s2 ->
    s1.node_type = s2.node_type &&
    List.length s1.children = List.length s2.children &&
    List.for_all2 nodes_structurally_equal s1.children s2.children
  | Placeholder p1, Placeholder p2 ->
    p1.kind = p2.kind && p1.index = p2.index
  | _ -> false

let templates_equal t1 t2 =
  match t1.structure, t2.structure with
  | (None, None), (None, None) -> true
  | (Some b1, None), (Some b2, None) -> nodes_structurally_equal b1 b2
  | (None, Some a1), (None, Some a2) -> nodes_structurally_equal a1 a2
  | (Some b1, Some a1), (Some b2, Some a2) ->
    nodes_structurally_equal b1 b2 && nodes_structurally_equal a1 a2
  | _ -> false

(** {1 Change inspection} *)

(** Get the node type of a change *)
let change_node_type change =
  match change.before, change.after with
  | Some (Structural { node_type; _ }), _ -> node_type
  | _, Some (Structural { node_type; _ }) -> node_type
  | Some (Placeholder p), _ -> p.original_text
  | _, Some (Placeholder p) -> p.original_text
  | None, None -> "unknown"

(** {1 Pretty printing} *)

let pp_placeholder_kind ppf = function
  | Identifier -> Format.fprintf ppf "id"
  | StringLit -> Format.fprintf ppf "str"
  | NumberLit -> Format.fprintf ppf "num"
  | TypeName -> Format.fprintf ppf "type"
  | Other -> Format.fprintf ppf "other"

let pp_placeholder ppf p =
  Format.fprintf ppf "$%a%d" pp_placeholder_kind p.kind p.index

let rec pp_abstract_node ppf = function
  | Structural { node_type; children } ->
    if children = [] then
      Format.fprintf ppf "%s" node_type
    else
      Format.fprintf ppf "(%s %a)"
        node_type
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
           pp_abstract_node) children
  | Placeholder p ->
    pp_placeholder ppf p

let node_to_string node =
  Format.asprintf "%a" pp_abstract_node node

let before_to_string change =
  Option.map node_to_string change.before

let after_to_string change =
  Option.map node_to_string change.after

let pp_abstract_change ppf change =
  let kind_str = match change.change_kind with
    | `Added -> "+"
    | `Removed -> "-"
    | `Modified -> "~"
    | `Replaced -> "!"
  in
  Format.fprintf ppf "%s " kind_str;
  (match change.before with
   | Some b -> Format.fprintf ppf "%a" pp_abstract_node b
   | None -> ());
  (match change.before, change.after with
   | Some _, Some _ -> Format.fprintf ppf " -> "
   | _ -> ());
  (match change.after with
   | Some a -> Format.fprintf ppf "%a" pp_abstract_node a
   | None -> ())

let to_string change =
  Format.asprintf "%a" pp_abstract_change change

(** {1 Grouping similar changes} *)

(** Group a list of abstract changes by their template *)
let group_by_template changes =
  let table = Hashtbl.create 16 in
  List.iter (fun change ->
    let template = to_template change in
    (* Find existing group or create new one *)
    let found = ref false in
    Hashtbl.iter (fun key group ->
      if not !found && templates_equal key template then begin
        Hashtbl.replace table key (change :: group);
        found := true
      end
    ) table;
    if not !found then
      Hashtbl.add table template [change]
  ) changes;
  (* Convert to list of groups *)
  Hashtbl.fold (fun _ group acc -> group :: acc) table []
  |> List.sort (fun a b -> compare (List.length b) (List.length a))
