open Tree

let print_tree tree =
  let rec loop n =
    match n with
    | Leaf l -> Printf.printf "%s" l.label
    | Node n ->
        Printf.printf "%s\n" n.label;
        let _ = List.map (fun c -> loop c) n.children in
        Printf.printf "\n";
        ()
  in
  loop tree
