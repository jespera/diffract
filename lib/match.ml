(** Pattern matching using concrete syntax with metavariables *)

include Match_types

let parse_pattern = Match_parse.parse_pattern

let parse_nested_pattern = Match_parse.parse_nested_pattern

let find_matches = Match_search.find_matches

let find_matches_in_file = Match_search.find_matches_in_file

let find_nested_matches = Match_search.find_nested_matches

let search = Match_search.search

let format_match = Match_search.format_match

let format_nested_match = Match_search.format_nested_match

let build_index = Match_search.build_index

let find_matches_with_index = Match_search.find_matches_with_index

let find_matches_multi = Match_search.find_matches_multi

let transform = Match_transform.transform

let transform_file = Match_transform.transform_file

let generate_diff = Match_transform.generate_diff

let apply_edits = Match_transform.apply_edits
