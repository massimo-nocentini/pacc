type 'a tree =
| Leaf
| Node of 'a tree * 'a * 'a tree;;

let rec count_leaves_of_tree = function
  | Node (Leaf, _, Leaf) -> 1
  | Node (left, _, right) ->
    (count_leaves_of_tree left) +
    (count_leaves_of_tree right)
  | _ -> 0;;

let rec count_height_of_tree = function
  | Node (Leaf, _, Leaf) -> 1
  | Node (left, _, right) ->
    let max_height =
      max (count_height_of_tree left) (count_height_of_tree right) in
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
  let mutable_creation_time = ref creation_time in
  let rec merge_trees_on_leftmost_leaf to_merge_tree =
    function
    | Leaf -> to_merge_tree
    | Node (Leaf, v, right) -> Node (to_merge_tree, v, right)
    | Node (left, v, right) ->
      Node (merge_trees_on_leftmost_leaf to_merge_tree left,
	    v,
	    right) in    
  let rec kernel inductive_tree =
    function
    | "" -> inductive_tree
    | brackets ->
      match find_rightmost_balance_breakpoint brackets with
      | 0 ->
	(* if we are here, the path hasn't breakpoint, that is, there
	   is no brackets to process after we manage this path *)
	let length_of_brackets = String.length brackets in
	let inner_brackets =
	  String.sub brackets 1 (length_of_brackets -2) in
	let time = !mutable_creation_time in
	mutable_creation_time := !mutable_creation_time + 1;
	merge_trees_on_leftmost_leaf
	  (Node (Leaf,
		 time,
		 (kernel
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
	       Leaf
	       brackets_on_the_right_of_breakpoint)
	    inductive_tree in
	kernel
  	  new_inductive_tree
  	  remaining_brackets
  in
  function brackets -> kernel Leaf brackets;;


let dot_of_tree_using_creation_time_as_node_label =
  let rec kernel parent_value =
    function
    | Leaf -> []
    | Node (left, value, right) ->
      let arrow = (string_of_int parent_value) ^
	" -> " ^ (string_of_int value) in
      let dot_of_left = kernel value left in
      let dot_of_right = kernel value right in
      arrow :: (dot_of_left @ dot_of_right)
  in
  function
  | Leaf -> []
  | Node (left, v, right) ->
    let dot_of_left = kernel v left in
    let dot_of_right = kernel v right in
    dot_of_left @ dot_of_right;;

let dot_of_tree initial_label =
  let mutable_label = ref initial_label in
  let make_arrow_string =
    fun source destination ->
      (string_of_int source) ^
	" -> " ^ (string_of_int destination) in
  let rec kernel =
    fun parent_label tree ->
    mutable_label := !mutable_label + 1;
    let current_label = !mutable_label in
    match tree with    
    | Leaf ->
      let arrow = make_arrow_string parent_label current_label in
      let make_invisible =
	function node_or_edge ->	  
	  let invisible_style_attribute = "[style=invis]" in
	  node_or_edge ^ " " ^ invisible_style_attribute
      in	  
      (make_invisible arrow) ::
	(make_invisible (string_of_int current_label)) :: []      
    | Node (left, _, right) ->
      let arrow = make_arrow_string parent_label current_label in
      let dot_of_left = kernel current_label left in
      let dot_of_right = kernel current_label right in
      arrow :: (dot_of_left @ dot_of_right)
  in
  function
  | Leaf -> []
  | Node (left, _, right) ->
    let dot_of_left = kernel initial_label left in
    let dot_of_right = kernel initial_label right in
    dot_of_left @ dot_of_right;;
