open QCheck2

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

let gen_trie =
  Gen.(sized (fun n -> list_size (0 -- n) int) |> map IntTrie.of_list)

let prop_assoc =
  Test.make ~name:"merge associative" ~count:200
    Gen.(triple gen_trie gen_trie gen_trie)
    (fun (a, b, c) ->
      IntTrie.equal
        (IntTrie.merge a (IntTrie.merge b c))
        (IntTrie.merge (IntTrie.merge a b) c) )

let prop_id =
  Test.make ~name:"merge identity" gen_trie (fun t ->
      IntTrie.equal (IntTrie.merge t IntTrie.neutral) t
      && IntTrie.equal (IntTrie.merge IntTrie.neutral t) t )

let prop_add =
  Test.make ~name:"add increases count"
    Gen.(pair int gen_trie)
    (fun (x, t) -> IntTrie.count x (IntTrie.add x t) = IntTrie.count x t + 1)

let prop_map_id =
  Test.make ~name:"map id = id" gen_trie (fun t ->
      IntTrie.equal (IntTrie.map (fun x -> x) t) t )

let prop_filter_true_false =
  Test.make ~name:"filter true/false" gen_trie (fun t ->
      IntTrie.equal (IntTrie.filter (fun _ -> true) t) t
      && IntTrie.equal (IntTrie.filter (fun _ -> false) t) IntTrie.neutral )

let prop_compare_equal =
  Test.make ~name:"compare equal coherence"
    Gen.(pair gen_trie gen_trie)
    (fun (a, b) ->
      let c = IntTrie.compare a b in
      c = 0 = IntTrie.equal a b )

let prop_length =
  Test.make ~name:"length equals sum counts" gen_trie (fun t ->
      let sum = IntTrie.fold_with_counts (fun acc (_, n) -> acc + n) 0 t in
      sum = IntTrie.length t )

let prop_to_list_with_counts =
  Test.make ~name:"to_list_with_counts valid" gen_trie (fun t ->
      let pairs = IntTrie.to_list_with_counts t in
      List.for_all (fun (_, n) -> n > 0) pairs
      && List.for_all (fun (x, n) -> IntTrie.count x t = n) pairs )

let prop_remove =
  Test.make ~name:"remove decreases until zero"
    Gen.(pair int gen_trie)
    (fun (x, t) ->
      let c = IntTrie.count x t in
      let rec apply k acc =
        if k <= 0 then acc else apply (k - 1) (IntTrie.remove x acc)
      in
      let t' = apply c t in
      IntTrie.count x t' = 0 )

let () =
  ignore
    (QCheck_base_runner.run_tests ~verbose:true
       [ prop_assoc
       ; prop_id
       ; prop_add
       ; prop_map_id
       ; prop_filter_true_false
       ; prop_compare_equal
       ; prop_length
       ; prop_to_list_with_counts
       ; prop_remove ] )
