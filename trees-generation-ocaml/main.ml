(** Entry point of the program*)
let _ =
  let datas_filename = Sys.argv.(1) in
  let ic = open_in datas_filename in
  print_string (datas_filename ^ "-printed from OCaml!");;
