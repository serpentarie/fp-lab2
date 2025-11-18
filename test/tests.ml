open OUnit2

module IntTrie = Prefix_tree.Make (struct
  type t = int

  let compare = Int.compare

  type component = int

  let compare_component = Int.compare

  let split n =
    if n = 0 then [0]
    else
      let rec aux acc x =
        if x = 0 then acc else aux ((x mod 10) :: acc) (x / 10)
      in
      if n < 0 then -1 :: aux [] (abs n) else aux [] n

  let join l =
    let rec aux acc = function
      | [] ->
          acc
      | h :: t ->
          aux ((acc * 10) + abs h) t
    in
    match l with -1 :: t -> -aux 0 t | _ -> aux 0 l
end)

let t = IntTrie.of_list [1; 1; 2; 10]

let test_fold _ =
  let s1 = IntTrie.fold_left (fun acc x -> acc ^ string_of_int x) "" t in
  let s2 = IntTrie.fold_right (fun x acc -> string_of_int x ^ acc) t "" in
  assert_equal "11210" s1 ; assert_equal "10121" s2

let test_add_count _ =
  let t' = IntTrie.add 5 t in
  assert_equal 1 (IntTrie.count 5 t') ;
  assert_equal 2 (IntTrie.count 1 t')

let test_remove _ =
  let t' = IntTrie.remove 1 t in
  assert_equal 1 (IntTrie.count 1 t') ;
  let t'' = IntTrie.remove 1 t' in
  assert_equal 0 (IntTrie.count 1 t'')

let test_merge _ =
  let a = IntTrie.of_list [1; 2] in
  let b = IntTrie.of_list [2; 2; 3] in
  let m = IntTrie.merge a b in
  assert_equal 1 (IntTrie.count 1 m) ;
  assert_equal 3 (IntTrie.count 2 m) ;
  assert_equal 1 (IntTrie.count 3 m)

let test_map _ =
  let m = IntTrie.map (fun x -> x + 1) t in
  assert_equal 0 (IntTrie.count 1 m) ;
  assert_equal 2 (IntTrie.count 2 m) ;
  assert_equal 1 (IntTrie.count 3 m)

let test_filter _ =
  let f = IntTrie.filter (fun x -> x mod 2 = 0) t in
  assert_equal [2; 10] (IntTrie.to_list f |> List.sort compare)

let test_equal_compare _ =
  let a = IntTrie.of_list [1; 2; 2] in
  let b = IntTrie.of_list [2; 1; 2] in
  let c = IntTrie.of_list [1; 1; 2] in
  assert_bool "a=b" (IntTrie.equal a b) ;
  assert_equal 0 (IntTrie.compare a b) ;
  assert_bool "a!=c" (not (IntTrie.equal a c)) ;
  assert_bool "cmp distinguishes" (IntTrie.compare a c <> 0)

let test_lists_counts_length _ =
  let a = IntTrie.of_list [1; 2; 2; 3] in
  let counts = IntTrie.to_list_with_counts a |> List.sort compare in
  assert_equal [(1, 1); (2, 2); (3, 1)] counts ;
  assert_equal 4 (IntTrie.length a)

let suite =
  "tests"
  >::: [ "fold" >:: test_fold
       ; "add" >:: test_add_count
       ; "remove" >:: test_remove
       ; "merge" >:: test_merge
       ; "map" >:: test_map
       ; "filter" >:: test_filter
       ; "equal_compare" >:: test_equal_compare
       ; "lists_counts_length" >:: test_lists_counts_length ]

let () = run_test_tt_main suite
