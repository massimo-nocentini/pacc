open TreesUtility;;
open BinaryTreeModule;;

(** Entry point of the program*)
let _ =
  let datas_filename = Sys.argv.(1) in
  let nodes_in_each_tree = Sys.argv.(2) in  
  let test_single_dot_repr =
    function
    | [] -> failwith "Now I don't want no data!"
    | first :: others ->
      let tree = make_tree_from_brackets 0 first.brackets in
      let dot_arrows = dot_of_tree 0 tree in
      let dot_string = List.fold_left
	(fun collected current -> collected ^ current ^ "; ")
	"" dot_arrows in
      print_string first.brackets;
      print_newline ();
      print_string dot_string
  in
  let parse_phase =
    function () ->
      print_string "Parsing .csv file...";
      let result = parse_data_file datas_filename (int_of_string nodes_in_each_tree) in
      print_string "\t\t[OK]";
      print_newline ();
      result
  in
  let dot_generation_phase =
    function updated_data_rows ->
      let dot_representation = dot_string_representation updated_data_rows in
      let csv_regexpr = Str.regexp_string ".csv" in      
      let dot_filename =
	Str.global_replace csv_regexpr ".dot" datas_filename in
      print_string "Generating .dot file...";
      write_string_to_file dot_representation dot_filename;
      print_string "\t\t[OK]";
      print_newline ();
      dot_representation
  in
  let augment_data_phase =
    function updated_data_rows ->
      let sample_time_title = wrap_string_with_double_quotes "keys" in
      let words_title = wrap_string_with_double_quotes "words" in
      let ones_title = wrap_string_with_double_quotes "ones" in
      let hits_title = wrap_string_with_double_quotes "hits" in
      let leaves_title = wrap_string_with_double_quotes "leaves" in
      let height_title = wrap_string_with_double_quotes "height" in
      let header_line =
	sample_time_title ^ "," ^
	  words_title ^ "," ^
	  ones_title ^ "," ^
	  hits_title ^ "," ^
	  leaves_title ^ "," ^
	  height_title ^ "\n" in
      let folding = fun collected current ->
	collected ^ (string_of_data_record current)
      in
      print_string "Augmenting data...";      
      let result = List.fold_left
	folding header_line updated_data_rows in
      print_string "\t\t[OK]";
      print_newline ();
      result
  in  
  let updated_data_rows = parse_phase () in
  let dot_representation = dot_generation_phase updated_data_rows in
  let augmented_content = augment_data_phase updated_data_rows in
  print_string augmented_content;
  (* test_single_dot_repr updated_data_rows; *)
  print_string (datas_filename ^ " with nodes " ^ nodes_in_each_tree);
  print_newline ();
  print_string dot_representation;
  print_newline ();;
