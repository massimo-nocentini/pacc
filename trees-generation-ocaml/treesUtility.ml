
type data_line = {
  sample_time: int;
  brackets: string;
  ones: string;
  hits: int
}
  

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
	hits = int_of_string (remove_quotes hits)
      }
    | _ -> failwith "We don't care about this case here!";;

let reading_test = function () ->
  let data_lines = read_file "TueDec11-19-30-42-2012.csv" in
  match data_lines with
  | header_line :: first_line :: other_lines ->
    parse_data_line_in_record first_line
  | _ -> failwith "We don't care about this case here!";;

