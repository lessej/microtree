let () =
  for i = 0 to Array.length Sys.argv - 1 do
    Printf.printf "[%i] %s\n" i Sys.argv.(i)
  done

type node = { label : string; children : tree list }
and leaf = { label : string }
and tree = Leaf of leaf | Node of node

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
