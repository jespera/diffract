/* C helper functions for OCaml bindings to tree-sitter */

#include <tree_sitter/api.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <stdlib.h>
#include <string.h>

/*
 * TSNode is a 32-byte struct passed by value in C, which doesn't work well
 * with OCaml FFI. We wrap it in a heap-allocated custom block.
 */
typedef struct {
    TSNode node;
    TSTree *tree;  /* Keep reference to prevent tree from being freed */
} TSNodeWrapper;

/* Custom block operations for TSNodeWrapper */
static void finalize_node_wrapper(value v) {
    (void)v; /* Node wrappers don't own the tree */
}

static struct custom_operations node_wrapper_ops = {
    "ts_node_wrapper",
    finalize_node_wrapper,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* Allocate a new node wrapper as an OCaml custom block */
static value alloc_node_wrapper(TSNode node, TSTree *tree) {
    CAMLparam0();
    CAMLlocal1(v);
    v = caml_alloc_custom(&node_wrapper_ops, sizeof(TSNodeWrapper), 0, 1);
    TSNodeWrapper *wrapper = (TSNodeWrapper *)Data_custom_val(v);
    wrapper->node = node;
    wrapper->tree = tree;
    CAMLreturn(v);
}

#define Node_wrapper_val(v) ((TSNodeWrapper *)Data_custom_val(v))

/* Custom block for TSTree */
static void finalize_tree(value v) {
    TSTree *tree = *(TSTree **)Data_custom_val(v);
    if (tree) {
        ts_tree_delete(tree);
    }
}

static struct custom_operations tree_ops = {
    "ts_tree",
    finalize_tree,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static value alloc_tree(TSTree *tree) {
    CAMLparam0();
    CAMLlocal1(v);
    /* Use memory hints to help GC understand external memory usage.
       Tree-sitter trees can use significant memory. We estimate ~8KB per tree
       as a reasonable average, with a max of 1MB before triggering GC. */
    v = caml_alloc_custom(&tree_ops, sizeof(TSTree *), 8192, 1048576);
    *(TSTree **)Data_custom_val(v) = tree;
    CAMLreturn(v);
}

#define Tree_val(v) (*(TSTree **)Data_custom_val(v))

/* ============ Parser functions ============ */

CAMLprim value ts_helper_parser_new(value v_unit) {
    CAMLparam1(v_unit);
    TSParser *parser = ts_parser_new();
    CAMLreturn(caml_copy_nativeint((intnat)parser));
}

CAMLprim value ts_helper_parser_delete(value v_parser) {
    CAMLparam1(v_parser);
    TSParser *parser = (TSParser *)Nativeint_val(v_parser);
    if (parser) {
        ts_parser_delete(parser);
    }
    CAMLreturn(Val_unit);
}

CAMLprim value ts_helper_parser_set_language(value v_parser, value v_language) {
    CAMLparam2(v_parser, v_language);
    TSParser *parser = (TSParser *)Nativeint_val(v_parser);
    const TSLanguage *language = (const TSLanguage *)Nativeint_val(v_language);
    bool success = ts_parser_set_language(parser, language);
    CAMLreturn(Val_bool(success));
}

/* ============ Tree functions ============ */

/* Parse and return a tree (with GC-managed lifetime) */
CAMLprim value ts_helper_parse(value v_parser, value v_source) {
    CAMLparam2(v_parser, v_source);

    TSParser *parser = (TSParser *)Nativeint_val(v_parser);
    const char *source = String_val(v_source);
    size_t source_len = caml_string_length(v_source);

    TSTree *tree = ts_parser_parse_string(parser, NULL, source, source_len);
    if (!tree) {
        caml_failwith("Parse failed");
    }

    CAMLreturn(alloc_tree(tree));
}

/* Get root node of a tree */
CAMLprim value ts_helper_tree_root_node(value v_tree) {
    CAMLparam1(v_tree);
    TSTree *tree = Tree_val(v_tree);
    TSNode root = ts_tree_root_node(tree);
    CAMLreturn(alloc_node_wrapper(root, tree));
}

/* ============ Node functions ============ */

/* Get the type of a node as a string */
CAMLprim value ts_helper_node_type(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    const char *type = ts_node_type(wrapper->node);
    CAMLreturn(caml_copy_string(type));
}

/* Get the S-expression representation */
CAMLprim value ts_helper_node_string(value v_node) {
    CAMLparam1(v_node);
    CAMLlocal1(v_result);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    char *str = ts_node_string(wrapper->node);
    v_result = caml_copy_string(str);
    free(str);
    CAMLreturn(v_result);
}

/* Check if node is named (vs anonymous like punctuation) */
CAMLprim value ts_helper_node_is_named(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    CAMLreturn(Val_bool(ts_node_is_named(wrapper->node)));
}

/* Check if node is null */
CAMLprim value ts_helper_node_is_null(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    CAMLreturn(Val_bool(ts_node_is_null(wrapper->node)));
}

/* Get child count (all children including anonymous) */
CAMLprim value ts_helper_node_child_count(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    uint32_t count = ts_node_child_count(wrapper->node);
    CAMLreturn(Val_int(count));
}

/* Get named child count */
CAMLprim value ts_helper_node_named_child_count(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    uint32_t count = ts_node_named_child_count(wrapper->node);
    CAMLreturn(Val_int(count));
}

/* Get child by index */
CAMLprim value ts_helper_node_child(value v_node, value v_index) {
    CAMLparam2(v_node, v_index);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    uint32_t index = Int_val(v_index);
    TSNode child = ts_node_child(wrapper->node, index);
    CAMLreturn(alloc_node_wrapper(child, wrapper->tree));
}

/* Get named child by index */
CAMLprim value ts_helper_node_named_child(value v_node, value v_index) {
    CAMLparam2(v_node, v_index);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    uint32_t index = Int_val(v_index);
    TSNode child = ts_node_named_child(wrapper->node, index);
    CAMLreturn(alloc_node_wrapper(child, wrapper->tree));
}

/* Get child by field name */
CAMLprim value ts_helper_node_child_by_field_name(value v_node, value v_name) {
    CAMLparam2(v_node, v_name);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    const char *name = String_val(v_name);
    uint32_t name_len = caml_string_length(v_name);
    TSNode child = ts_node_child_by_field_name(wrapper->node, name, name_len);
    CAMLreturn(alloc_node_wrapper(child, wrapper->tree));
}

/* Get field name for child at index (returns None if no field name) */
CAMLprim value ts_helper_node_field_name_for_child(value v_node, value v_index) {
    CAMLparam2(v_node, v_index);
    CAMLlocal1(v_some);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    uint32_t index = Int_val(v_index);
    const char *name = ts_node_field_name_for_child(wrapper->node, index);
    if (name) {
        v_some = caml_alloc(1, 0);  /* Some */
        Store_field(v_some, 0, caml_copy_string(name));
        CAMLreturn(v_some);
    } else {
        CAMLreturn(Val_int(0));  /* None */
    }
}

/* Get parent node */
CAMLprim value ts_helper_node_parent(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    TSNode parent = ts_node_parent(wrapper->node);
    CAMLreturn(alloc_node_wrapper(parent, wrapper->tree));
}

/* Get next sibling */
CAMLprim value ts_helper_node_next_sibling(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    TSNode sibling = ts_node_next_sibling(wrapper->node);
    CAMLreturn(alloc_node_wrapper(sibling, wrapper->tree));
}

/* Get previous sibling */
CAMLprim value ts_helper_node_prev_sibling(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    TSNode sibling = ts_node_prev_sibling(wrapper->node);
    CAMLreturn(alloc_node_wrapper(sibling, wrapper->tree));
}

/* Get next named sibling */
CAMLprim value ts_helper_node_next_named_sibling(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    TSNode sibling = ts_node_next_named_sibling(wrapper->node);
    CAMLreturn(alloc_node_wrapper(sibling, wrapper->tree));
}

/* Get previous named sibling */
CAMLprim value ts_helper_node_prev_named_sibling(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    TSNode sibling = ts_node_prev_named_sibling(wrapper->node);
    CAMLreturn(alloc_node_wrapper(sibling, wrapper->tree));
}

/* ============ Position functions ============ */

CAMLprim value ts_helper_node_start_byte(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    CAMLreturn(Val_int(ts_node_start_byte(wrapper->node)));
}

CAMLprim value ts_helper_node_end_byte(value v_node) {
    CAMLparam1(v_node);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    CAMLreturn(Val_int(ts_node_end_byte(wrapper->node)));
}

/* Returns (row, column) as a pair */
CAMLprim value ts_helper_node_start_point(value v_node) {
    CAMLparam1(v_node);
    CAMLlocal1(v_pair);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    TSPoint point = ts_node_start_point(wrapper->node);
    v_pair = caml_alloc_tuple(2);
    Store_field(v_pair, 0, Val_int(point.row));
    Store_field(v_pair, 1, Val_int(point.column));
    CAMLreturn(v_pair);
}

CAMLprim value ts_helper_node_end_point(value v_node) {
    CAMLparam1(v_node);
    CAMLlocal1(v_pair);
    TSNodeWrapper *wrapper = Node_wrapper_val(v_node);
    TSPoint point = ts_node_end_point(wrapper->node);
    v_pair = caml_alloc_tuple(2);
    Store_field(v_pair, 0, Val_int(point.row));
    Store_field(v_pair, 1, Val_int(point.column));
    CAMLreturn(v_pair);
}

/* ============ Legacy function for backward compatibility ============ */

CAMLprim value ts_helper_parse_to_sexp(value v_parser, value v_source) {
    CAMLparam2(v_parser, v_source);
    CAMLlocal1(v_result);

    TSParser *parser = (TSParser *)Nativeint_val(v_parser);
    const char *source = String_val(v_source);
    size_t source_len = caml_string_length(v_source);

    TSTree *tree = ts_parser_parse_string(parser, NULL, source, source_len);
    if (!tree) {
        caml_failwith("Parse failed");
    }

    TSNode root = ts_tree_root_node(tree);
    char *sexp = ts_node_string(root);

    v_result = caml_copy_string(sexp);

    free(sexp);
    ts_tree_delete(tree);

    CAMLreturn(v_result);
}
