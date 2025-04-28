open Tree

let chars_of_string str = List.init (String.length str) (String.get str)

let split_string_at_idxs str idxs =
  let rec iter_pairs pairs splits =
    match pairs with
    | [] -> splits
    | (s, e) :: rem ->
        let len = e - s in
        let sub = String.sub str s len in
        let splits = if String.equal sub "" then splits else sub :: splits in
        iter_pairs rem splits
  in
  iter_pairs idxs [] |> List.map (fun str -> String.trim str)

let split_on_level inpt =
  let rec loop chars i prev lvl coms =
    match chars with
    | [] ->
        if lvl = 0 then (prev, String.length inpt) :: coms
        else failwith "Malformed tree: mismatched parentheses."
    | ',' :: rem ->
        if lvl = 0 then loop rem (i + 1) (i + 1) 0 ((prev, i) :: coms)
        else loop rem (i + 1) prev lvl coms
    | ')' :: rem -> loop rem (i + 1) prev (lvl - 1) coms
    | '(' :: rem -> loop rem (i + 1) prev (lvl + 1) coms
    | _ :: rem -> loop rem (i + 1) prev lvl coms
  in
  let chars = chars_of_string inpt in
  let split_idxs = loop chars 0 0 0 [] in
  split_string_at_idxs inpt split_idxs

let into_tree_parts tree =
  match String.index_opt tree '(' with
  | Some s -> (
      let len = String.length tree in
      let children = String.sub tree (s + 1) (len - s - 2) |> String.trim in
      let children =
        match children with "" -> None | children -> Some children
      in
      let root = String.sub tree 0 s |> String.trim in
      match root with
      | "" -> failwith "Malformed tree.  This tree has no root."
      | root -> (root, children))
  | None -> (tree, None)

let build_trees inpt =
  let rec loop tree =
    let root, children = into_tree_parts tree in
    match children with
    | None -> Leaf { label = root }
    | Some children ->
        let subtrees =
          split_on_level children
          |> List.map (fun t -> String.trim t)
          |> List.map (fun t -> loop t)
        in
        Node { label = root; children = subtrees }
  in
  split_on_level inpt |> List.map (fun t -> loop t)
