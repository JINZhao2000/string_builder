open OUnit2
open String_builder

let data = concat (concat (word "ab") (concat (word "cde") (concat (word "fg") (word "hij")))) (concat (concat (word "kl") (word "mno")) (word "pq"))

let suite = 
  "String_Builder" >::: [
    "char_at_-1" >:: (fun _ -> assert_raises (Invalid_argument "index out of bounds") (fun() ->(char_at data (-1))));
    "char_at_0" >:: (fun _ -> assert_equal (char_at data 0) 'a');
    "char_at_9" >:: (fun _ -> assert_equal (char_at data 9) 'j');
    "char_at_10" >:: (fun _ -> assert_equal (char_at data 10) 'k');
    "char_at_16" >:: (fun _ -> assert_equal (char_at data 16) 'q');
    "char_at_17" >:: (fun _ -> assert_raises (Invalid_argument "index out of bounds") (fun() ->(char_at data (17))));

    "list_of_string" >:: (fun _ -> assert_equal (list_of_string data) ["ab";"cde";"fg";"hij";"kl";"mno";"pq"]);

    "sub_string_left" >:: (fun _ -> assert_equal (list_of_string (sub_string data 1 5)) ["b"; "cde"; "f"]);
    "sub_string_right" >:: (fun _ -> assert_equal (list_of_string (sub_string data 11 5)) ["l"; "mno"; "p"]);
    "sub_string_cross" >:: (fun _ -> assert_equal (list_of_string (sub_string data 6 7)) ["g"; "hij"; "kl"; "m"]);
    "sub_string_exni-1" >:: (fun _ -> assert_raises (Invalid_argument "Index out of bound") (fun() ->(sub_string data (-1) 4)));
    "sub_string_exnm0" >:: (fun _ -> assert_raises (Invalid_argument "Length should be positive") (fun() ->(sub_string data 1 0)));
    "sub_string_exnmnl" >:: (fun _ -> assert_raises (Invalid_argument "Length of substring out of bound") (fun() ->(sub_string data 5 13)));

    "cost" >:: (fun _ -> assert_equal (cost data) 52);
    "balance" >:: (fun _ -> assert_bool "not balanced" ((cost (balance data)) <= (cost data)))
  ]

let _ = run_test_tt_main suite