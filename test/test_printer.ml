open OUnit2
open Microtree.Parser
open Microtree.Printer

let test_print_tree _ =
  let tree_str =
    "root (node 1 (leaf 1, leaf 2), node 2 (leaf 3, leaf 4, leaf 5, leaf 6))"
  in
  let trees = build_trees tree_str in
  List.iter (fun t -> postorder_tree t |> print_tree) trees

(* assert_equal 0 0  *)

let test_print_tree_suite =
  "MicrotreeTest print_tree" >::: [ "basic" >:: test_print_tree ]

let () = run_test_tt_main test_print_tree_suite
