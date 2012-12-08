
type node = {
  mutable creation_time: int;
  mutable parent: int;
  mutable left: int;
  mutable right: int };;

let random_tree_generator =
  function number_of_internal_nodes ->
    let array_dimension = (number_of_internal_nodes * 2) + 1 in
    let array_init_function =
      function i ->
	{ creation_time = 0; parent = -1; left = -1; right = -1 } in
    let tree = Array.init array_dimension array_init_function in
    let root = ref 0 in
    
    Random.self_init ();
    let i = ref 1 in
    let limit = 2 * number_of_internal_nodes in
    while !i < limit do
      
      let hit = Random.int !i in
      let direction = Random.bool () in
      let parent = tree.(hit).parent in
      
      if parent = -1
      then root := !i
      else
	if tree.(parent).left = hit
	then tree.(parent).left <- !i
	else tree.(parent).right <- !i;
      
      tree.(!i).parent <- parent;
      tree.(!i).creation_time <- !i;
      
      if direction
      then
	begin
	  tree.(!i).left <- !i + 1;
	  tree.(!i).right <- hit
	end
      else
	begin
	  tree.(!i).left <- hit;
	  tree.(!i).right <- !i + 1
	end;
      
      tree.(hit).parent <- !i;
      tree.(!i + 1).parent <- !i;
      tree.(!i + 1).creation_time <- !i+1;
      
      i := !i + 2
    done;
    
    tree
;;

let find_root_node = 
  function tree ->
    let is_root_node = function node ->
      match node with
      | { parent = -1 } -> true
      | _ -> false in
    let tree_as_list = Array.to_list tree in
    List.find is_root_node tree_as_list;;	

(* let generated_tree = random_tree_generator 5 in *)
(* in find_root_node generated_tree;; *)
let string_of_tree tree =
  let root_node = find_root_node tree in
  let strings = ref [] in
  let rec string_of_node = function node ->    
    if node.left <> -1
    then
      let left_node = Array.get tree node.left in
      strings :=
	((string_of_int node.creation_time) ^
	    " -> " ^ (string_of_int left_node.creation_time)) :: !strings;
      string_of_node left_node;
    else ();
    
    if node.right <> -1
    then
      let right_node = Array.get tree node.right in
      strings :=
	((string_of_int node.creation_time) ^
	  " -> " ^ (string_of_int right_node.creation_time)) :: !strings;
	string_of_node right_node
    else ();

  in
  string_of_node root_node;
  !strings;;

let build_dot_representation tree =
  let relation = string_of_tree tree in
  let folding = fun collected current ->
    collected ^ current ^ "; " in  
  let almost_complete = List.fold_left folding "digraph{ " relation in
  almost_complete ^ "}";;
    

    

      
    
  


