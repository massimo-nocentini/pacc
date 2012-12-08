
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
				(* tree.(hit).creation_time <- !i; *)
				
				i := !i + 2
			done;
			
			tree
;;

