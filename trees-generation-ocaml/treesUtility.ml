open BinaryTreeModule;;

type data_line = {
  sample_time: int;
  brackets: string;
  ones: string;
  hits: int;
  mutable leaves: int;
  mutable height: int;
  mutable dot_arrows: string list
};;
  

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let parse_data_line_in_record =
  let remove_quotes = function string ->
    let quotes_regexpr = Str.regexp_string "\"" in
    Str.global_replace quotes_regexpr "" string in
  let adjust_brackets = function brackets ->
    let blank_regexpr = Str.regexp_string " " in
    let brackets_without_blanks = 
      Str.global_replace blank_regexpr "" brackets in
    remove_quotes brackets_without_blanks
  in
  function data_line_as_string ->
    let separator_regexpr = Str.regexp_string "," in
    let columns = Str.split separator_regexpr data_line_as_string in
    match columns with
    | sample_time :: brackets :: ones :: hits :: empty ->
      let adjusted_brackets = adjust_brackets brackets in
      {
	sample_time = int_of_string (remove_quotes sample_time);
	brackets = adjusted_brackets;
	ones = (remove_quotes ones);
	hits = int_of_string (remove_quotes hits);
	leaves = -1;
	height = -1;
	dot_arrows = []
      }
    | _ -> failwith "We don't care about this case here!";;

let parse_data_file = fun filename nodes_in_each_tree ->
  let data_lines = read_file filename in
  let rec kernel rows =
    function
    | [] -> rows
    | current_line :: other_lines ->
      let row = parse_data_line_in_record current_line in
      let initial_node_label = (List.length rows) *
	((2*nodes_in_each_tree) + 1) in
      let binary_tree = make_tree_from_brackets initial_node_label row.brackets in
      row.leaves <- count_leaves_of_tree binary_tree;
      row.height <- count_height_of_tree binary_tree;
      row.dot_arrows <- dot_of_tree initial_node_label binary_tree;
      kernel (row :: rows) other_lines
  in
  match data_lines with
  | [] -> failwith "No line present to parse!"
  | header_line :: other_lines ->
    List.rev (kernel [] other_lines);;

let reading_test = function () ->
  parse_data_file "TueDec11-19-30-42-2012.csv" 4;;

let dot_string_representation =
  let empty_label = "\"" in
  let dot_preamble = "digraph { edge [arrowsize=.5, fontsize=8];\
 	node [label=" ^ empty_label ^ empty_label ^
    ", shape=circle,height=0.12,width=0.12,fontsize=10]; " in
  let folding = fun collected current ->
    let joined_arrows =
      List.fold_left
	(fun other_arrows current_arrow ->
	  other_arrows ^ current_arrow ^ "; ")
	""
	current.dot_arrows
    in
    collected ^ joined_arrows
  in
  function datarows ->
    let almost_complete = List.fold_left
      folding dot_preamble datarows in
    almost_complete ^ "}";;

let write_string_to_file =
  fun content filename ->
    let channel = open_out filename in
    output channel content 0 (String.length content);
    close_out channel;;

let string_of_data_record =
  let folding = fun collected current ->
    collected ^ "," ^ current
  in
  function
  |
      {
	sample_time = sample_time;
	brackets = brackets;
	ones = ones;
	hits = hits;
	leaves = leaves;
	height = height;
	dot_arrows = dot_arrows
      } ->
    let line_as_list = 
      (string_of_int sample_time) ::
	brackets ::
	ones ::
	(string_of_int hits) ::
	(string_of_int leaves) ::
	(string_of_int height) :: [] in
    let line = (List.fold_left folding "" line_as_list) ^ "\n" in
    Str.string_after line 1;;

let wrap_string_with_double_quotes =
  function string ->
    let double_quotes = "\"" in
    double_quotes ^ string ^ double_quotes;;
