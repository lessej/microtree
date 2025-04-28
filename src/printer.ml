open Tree

(* let print_tree tree = *)
(*   let rec loop n = *)
(*     match n with *)
(*     | Leaf l -> Printf.printf "%s" l.label *)
(*     | Node n -> *)
(*         Printf.printf "%s\n" n.label; *)
(*         let _ = List.map (fun c -> loop c) n.children in *)
(*         Printf.printf "\n"; *)
(*         () *)
(*   in *)
(*   loop tree *)

let postorder_tree tree =
  let rec postorder acc = function
    | Leaf l -> l.label :: acc
    | Node n ->
        let subtrees =
          List.map (fun c -> postorder acc c) n.children |> List.concat
        in
        n.label :: subtrees
  in
  postorder [] tree

let print_tree tl =
  Printf.printf "Printing tree..\n";
  List.iter (fun n -> Printf.printf "%s," n) tl

(*

traverse the tree postorder


*)
