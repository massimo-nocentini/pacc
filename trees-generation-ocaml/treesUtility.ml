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
  let rec kernel creation_time inductive_tree =
    function
    | "" -> inductive_tree
    (* | "()" -> *)
    (*   kernel *)
    (* 	(succ creation_time) *)
    (* 	(merge_trees_on_leftmost_leaf *)
    (* 	   (Node (Leaf, creation_time, Leaf)) *)
    (* 	   inductive_tree) *)
    (* 	""	 *)
    | brackets ->
      match find_rightmost_balance_breakpoint brackets with
      | 0 ->
	let length_of_brackets = String.length brackets in
	let inner_brackets =
	  String.sub brackets 1 (length_of_brackets -2) in
	merge_trees_on_leftmost_leaf
	  (kernel
  	     (succ creation_time)
  	     Leaf
  	     inner_brackets)
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
	  remaining_brackets in

	
      (* 	kernel *)
      (* 	  (succ creation_time) *)
      (* 	  (merge_trees_on_leftmost_leaf *)
      (* 	     (kernel *)
      (* 		(succ creation_time) *)
      (* 		Leaf *)
      (* 		brackets_on_the_right_of_breakpoint) *)
      (* 	     new_inductive_tree) *)
      (* 	  remaining_brackets in *)
  
	
      (* kernel *)
      (* 	(succ creation_time) *)
      (* 	(merge_trees_on_leftmost_leaf *)
      (* 	   (kernel *)
      (* 	      (succ creation_time) *)
      (* 	      Leaf *)
      (* 	      brackets_on_the_right_of_breakpoint) *)
      (* 	   inductive_tree) *)
      (* 	remaining_brackets in *)
  

  
  (* match (find_rightmost_balance_breakpoint brackets) with *)
  (* | 0 -> *)
  (*   let length_of_brackets = String.length brackets in *)
  (*   let inner_brackets = String.sub brackets 1 (length_of_brackets -2) in *)
  (*   Node (Leaf, *)
  (* 	creation_time, *)
  (* 	make_tree_from_brackets *)
  (* 	  (succ creation_time) *)
  (* 	  inductive_tree *)
  (* 	  inner_brackets) *)
  (* | rightmost_balance_breakpoint -> *)
  (*   let brackets_on_the_right_of_breakpoint = *)
  (*     Str.string_after brackets rightmost_balance_breakpoint in *)
  (*   let second_rightmost_balance_breakpoint = *)
  (*     find_rightmost_balance_breakpoint  *)
  (*       (Str.string_before brackets rightmost_balance_breakpoint) in *)
  (*   let brackets_between_second_and_first_rightmost_breakpoints = *)
  (*     String.sub brackets second_rightmost_balance_breakpoint *)
  (*       (rightmost_balance_breakpoint - second_rightmost_balance_breakpoint) in *)
  (*   let tree_of_brackets_between_breakpoints = *)
  (*     make_tree_from_brackets *)
  (*       creation_time *)
  (*       inductive_tree *)
  (*       brackets_between_second_and_first_rightmost_breakpoints in *)
  (*   let tree_of_rightmost_brackets_before_breakpoint = *)
  (*     make_tree_from_brackets *)
  (*       (succ creation_time) *)
  (*       inductive_tree *)
  (*       brackets_on_the_right_of_breakpoint in *)
  (* (\* Node( Leaf, brackets_between_second_and_first_rightmost_breakpoints, Leaf) *\) *)
  (*   let merged_tree = merge_trees_on_leftmost_leaf *)
  (*     tree_of_brackets_between_breakpoints *)
  (*     tree_of_rightmost_brackets_before_breakpoint in *)
  (*   let remaining_brackets = Str.string_before *)
  (*     brackets second_rightmost_balance_breakpoint in *)
  (*   make_tree_from_brackets *)
  (*     (succ creation_time) *)
  (*     merged_tree *)
  (*     remaining_brackets in *)
  function brackets -> kernel creation_time Leaf brackets 
(* | "" -> failwith "is it correct to fail here?" *)
(* | "()" -> Node (Leaf, creation_time, Leaf) *)
(* | brackets -> *)
(*   match (find_rightmost_balance_breakpoint brackets) with *)
(*   | 0 -> *)
(*     let length_of_brackets = String.length brackets in *)
(*     let inner_brackets = String.sub brackets 1 (length_of_brackets -2) in *)
(*     Node (Leaf, *)
(* 	    creation_time, *)
(* 	    make_tree_from_brackets *)
(* 	      (succ creation_time) *)
(* 	      inductive_tree *)
(* 	      inner_brackets) *)
(*   | rightmost_balance_breakpoint -> *)
(*     let brackets_on_the_right_of_breakpoint = *)
(* 	Str.string_after brackets rightmost_balance_breakpoint in *)
(*     kernel creation_time Leaf brackets *)
and merge_trees_on_leftmost_leaf to_merge_tree =
  function
  | Leaf -> Node (Leaf, 0, to_merge_tree)
  | Node (Leaf, v, right) -> Node (to_merge_tree, v, right)
  | Node (left, v, right) ->
    Node (merge_trees_on_leftmost_leaf to_merge_tree left,
	  v,
	  right);;
 (*  | _ -> failwith "Impossible merge a tree into a Leaf object \ *)
 (* (it hasn't enough room!), a Node is required"; *)
    
    






