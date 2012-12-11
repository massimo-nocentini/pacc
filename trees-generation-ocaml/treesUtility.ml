type myType = Cons | List;;

let _ =
  let datas_filename = Sys.argv.(1) in
  print_string (datas_filename ^ "-printed from OCaml!");;
