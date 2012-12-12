open TreesUtility;;
open BinaryTreeModule;;

(** Entry point of the program*)
let _ = 
  let datas_filename = Sys.argv.(1) in
  let nodes_in_each_tree = Sys.argv.(2) in
  let updated_data_rows =
    parse_data_file datas_filename (int_of_string nodes_in_each_tree) in
  let dot_representation = dot_string_representation updated_data_rows in
  match updated_data_rows with
  | [] -> failwith "Now I don't want no data!"
  | first :: others ->
    let tree = make_tree_from_brackets 0 first.brackets in
    let dot_arrows = dot_of_tree tree in
    let dot_string = List.fold_left
	(fun collected current -> collected ^ current ^ "; ")
	"" dot_arrows in
    print_string first.brackets;
    print_newline ();
    print_string dot_string;

    print_newline ();
    print_string (datas_filename ^ " with nodes " ^ nodes_in_each_tree);
    print_newline ();
    print_string dot_representation;
    print_newline ();;
