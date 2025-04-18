let () =
  for i = 0 to Array.length Sys.argv - 1 do
    Printf.printf "[%i] %s\n" i Sys.argv.(i)
  done

type node = {
  label: string;
  children: tree list
}
and leaf = {
  label: string;
}
and tree =
  | Leaf of leaf
  | Node of node

let chars_of_string str = List.init (String.length str) (String.get str)

let split_string_at_idxs str idxs =
  let rec iter_pairs pairs splits =
    match pairs with
    | [] -> splits
    | (s,e)::rem ->
        let len = e-s in
        let sub = String.sub str s len in
        let splits = 
          if String.equal sub "" then splits
          else (sub::splits) 
        in
        iter_pairs rem splits
  in
  iter_pairs idxs []
    |> List.map (fun s -> String.trim s)


let split_on_level inpt =
  let rec loop chars i p lvl coms =
    match chars with
    | [] -> ((p, String.length inpt)::coms)
    | ','::rem ->
        if lvl = 0 then 
          let rem = List.tl rem in
          loop rem (i+2) (i+2) 0 ((p,i)::coms) 
        else
          loop rem (i+1) p lvl coms 
    | ')'::rem -> loop rem (i+1) p (lvl-1) coms
    | '('::rem -> loop rem (i+1) p (lvl+1) coms
    | _::rem -> loop rem (i+1) p lvl coms
  in
  let chars = chars_of_string inpt in
  let split_idxs = loop chars 0 0 0 [] in
  split_string_at_idxs inpt split_idxs




(*
tree: root (mid_1, mid_2 (leaf1, leaf 2, leaf 3), mid_3), root 2
root (mid_1 | mid_2 (leaf1, 

root (mid 1, mid 2 (leaf 1, leaf 2, leaf 3), mid 3)


1: split into trees
2: split into parent and children root / ()
3: if there is no () then it's a leaf

recursive function return tree
make this level and then return it
as the recursion goes back up it will complete the tree







tree: (root (mid_1 (leaf1 leaf2 leaf3) mid_2 (leaf4, leaf5)))
tree2: (root (mid_1 (leaf1 leaf2 leaf3)))
tree3: (root (mid_1 mid_2 mid_3))

                      root
                ________|__________
                |                 |
              mid_1             mid_2
          ______|______        ___|___
          |     |     |        |     |
        leaf1 leaf2 leaf3     leaf4 leaf5

node {
  parent: node
  children: node[]
  label: string
}

(->
root

(->
mid_1

(->
leaf1, leaf2, leaf3
) ->
nodeleaf1, nodeleaf2, nodeleaf3
)->
mid_1
*)

