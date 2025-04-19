open OUnit2
open Microtree.Parser
open Microtree.Tree

let test_split_string_at_idxs _ =
  let test_string = "Hello, World!" in
  let res = split_string_at_idxs test_string [(0,1);(2,5);(6,13)] in
  let expected = ["World!"; "llo"; "H"] in
  assert_equal res expected

let test_no_split _ =
  let test_string = "Hello, World!" in
  let res = split_string_at_idxs test_string [] in
  let expected = [] in
  assert_equal res expected

let test_beginning_split _ =
  let test_string = "Hello, World!" in
  let res = split_string_at_idxs test_string [(0,13)] in
  let expected = ["Hello, World!"] in
  assert_equal res expected

let split_str_at_idx_suite =
  "MicrotreeTest split_string_at_idxs" >::: [
    "basic" >:: test_split_string_at_idxs;
    "no split" >:: test_no_split;
    "beginning split" >:: test_beginning_split;
  ]

let test_chars_of_str _ =
  let test_string = "Hello, World!" in
  let res = chars_of_string test_string in
  let expected = ['H';'e';'l';'l';'o';',';' ';'W';'o';'r';'l';'d';'!'] in
  assert_equal res expected

let test_chars_of_empty_str _ = 
  let test_string = "" in
  let res = chars_of_string test_string in
  let expected = [] in
  assert_equal res expected

let chars_of_str_suite =
  "MicrotreeTest chars_of_string" >::: [
    "basic" >:: test_chars_of_str;
    "empty" >:: test_chars_of_empty_str
  ]

let test_split_level _ = 
  let test_trees tree expected =
    let test_string = tree in
    let res = split_on_level test_string in
    assert_equal res expected
  in
  test_trees "root (level1_1, level1_2 (level2_1, level2_2))" ["root (level1_1, level1_2 (level2_1, level2_2))"];
  test_trees "root_1, root_2" ["root_1"; "root_2"]

let split_on_level_suite =
  "MicrotreeTest split_on_level" >::: [
    "basic" >:: test_split_level;
  ]

let test_into_tree_parts _ =
  let test_tree tree expected =
    let (root, children) = into_tree_parts tree in
    assert_equal (root, children) expected
  in
  test_tree "root" ("root", None);
  test_tree "root (leaf, leaf)" ("root", Some "leaf, leaf") 

let into_tree_parts_suite =
  "MicrotreeTest into_tree_parts" >::: [
    "basic" >:: test_into_tree_parts;
  ]

let test_build_trees _ =
  let expected = Node {
    label = "root";
    children = Node {
      label = "level 1";
      children = Leaf { label = "leaf 1" }::Leaf { label = "leaf 2"}::[]
    }::Leaf { label = "level 1 leaf" }::[]
  }::[] in
  let tree_str = "root (level 1 (leaf 1, leaf 2), level 1 leaf)" in
  let res = build_trees tree_str in
  assert_equal res expected

let test_build_multiple_trees _ =
  let expected = Node {
    label = "root 1";
    children = Leaf { label = "leaf 1" }::Leaf { label = "leaf 2" }::[]
  }::Node {
    label = "root 2";
    children = Leaf { label = "leaf 3" }::Leaf { label = "leaf 4" }::[]
  }::[] in
  let tree_str = "root 1    (    leaf 1 , leaf 2),root 2(leaf 3, leaf 4)" in
  let res = build_trees tree_str in
  assert_equal res expected

let build_trees_suite =
  "MicrotreeTest build_trees" >::: [
    "basic" >:: test_build_trees;
    "multiple" >:: test_build_multiple_trees;
  ]

let () =
  run_test_tt_main split_str_at_idx_suite;
  run_test_tt_main chars_of_str_suite;
  run_test_tt_main split_on_level_suite;
  run_test_tt_main into_tree_parts_suite;
  run_test_tt_main build_trees_suite
