open OUnit

(* let test_size_for_recursive_lists _ =                                     *)
(*   assert_equal 4 (specialized_size [1;2;3;4]);                            *)
(*   let rec l1 = 1 :: 2 :: l2                                               *)
(*   and l2 = 3 :: 1 :: 5 :: l1                                              *)
(*   in assert_equal 5 (specialized_size l1)                                 *)

(* let desktop_calculator_test_manually _ =                                  *)
(*   let initial_state = {lcd=0; lka=Equals; loa=Equals; vpr=0} in           *)
(*   let state2 = transition initial_state (Digit 3) in                      *)
(*   let state3 = transition state2 Plus in                                  *)
(*   let state4 = transition state3 (Digit 2) in                             *)
(*   let state5 = transition state4 (Digit 1) in                             *)
(*   let state6 = transition state5 Times in                                 *)
(*   let state7 = transition state6 (Digit 2) in                             *)
(*   let state8 = transition state7 (Equals) in                              *)
(*   assert_equal 48 state8.vpr;;                                            *)

(* let desktop_calculator_test_folding _ =                                   *)
(*   let initial_state = {lcd=0; lka=Equals; loa=Equals; vpr=0}              *)
(*   and list = [Digit 3; Plus; Digit 2; Digit 1; Times; Digit 2; Equals] in *)
(*   let final_state = folding_transition initial_state list in              *)
(*   assert_equal 48 final_state.vpr;;                                       *)

let suite = "Recursive structure's that aren't function"
  >::: [];;
    (* "test_size_for_recursive_lists" >:: test_size_for_recursive_lists;       *)
    (* "desktop_calculator_test_manually" >:: desktop_calculator_test_manually; *)
    (* "desktop_calculator_test_folding" >:: desktop_calculator_test_folding];; *)
	
let _ = run_test_tt_main suite;;
