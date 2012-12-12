(** Entry point of the program*)
(* let _ = *)
(*   let datas_filename = Sys.argv.(1) in *)
(*   print_string (datas_filename ^ "-printed from OCaml!");; *)

type 'a tree =
| Leaf
| Node of 'a tree * 'a * 'a tree;;

let rec count_leaves_in_tree = function
  | Node (Leaf, _, Leaf) -> 1
  | Node (left, _, right) ->
    (count_leaves_in_tree left) +
    (count_leaves_in_tree right)
  | _ -> 0;;

let rec height_of_tree = function
  | Node (Leaf, _, Leaf) -> 1
  | Node (left, _, right) ->
    let max_height =
      max (height_of_tree left) (height_of_tree right) in
    1 + max_height
  | _ -> 0;;

let rec find_rightmost_balance_breakpoint = 
  let rec kernel initial_sum =
    function
    | "" -> 0
    | brackets -> 
      let last_index = (String.length brackets) - 1 in
      let last_char = brackets.[last_index] in
      let cumulation = initial_sum +
	(if last_char = '(' then 1 else -1) in
      if cumulation = 0
      then last_index
      else kernel
	cumulation
	(Str.string_before brackets last_index) in
  function brackets -> kernel 0 brackets;;

let rec make_tree_from_brackets creation_time =
  let rec merge_trees_on_leftmost_leaf to_merge_tree =
    function
    | Leaf -> to_merge_tree
    | Node (Leaf, v, right) -> Node (to_merge_tree, v, right)
    | Node (left, v, right) ->
      Node (merge_trees_on_leftmost_leaf to_merge_tree left,
	    v,
	    right) in    
  let rec kernel creation_time inductive_tree =
    function
    | "" -> inductive_tree
    | brackets ->
      match find_rightmost_balance_breakpoint brackets with
      | 0 ->
	let length_of_brackets = String.length brackets in
	let inner_brackets =
	  String.sub brackets 1 (length_of_brackets -2) in
	(* if we are here, the path hasn't breakpoint, that is, there
	   is no brackets to process after we manage this path *)
	merge_trees_on_leftmost_leaf
	  (Node (Leaf,
		 creation_time,	      
		 (kernel
  		    (succ creation_time)
  		    Leaf
  		    inner_brackets)))
	  inductive_tree
      | rightmost_balance_breakpoint ->
	let brackets_on_the_right_of_breakpoint =
	  Str.string_after brackets rightmost_balance_breakpoint in
	let remaining_brackets = Str.string_before
	  brackets rightmost_balance_breakpoint in
	let new_inductive_tree = 
	  merge_trees_on_leftmost_leaf
	    (kernel
	       (succ creation_time)
	       Leaf
	       brackets_on_the_right_of_breakpoint)
	    inductive_tree in
	kernel
  	  (succ creation_time)
  	  new_inductive_tree
  	  remaining_brackets
  in
  function brackets -> kernel creation_time Leaf brackets;;






