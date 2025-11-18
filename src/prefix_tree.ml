module type Elt = sig
  type t

  val compare : t -> t -> int

  type component

  val compare_component : component -> component -> int

  val split : t -> component list

  val join : component list -> t
end

module type S = sig
  type elt

  type t

  val empty : t

  val neutral : t

  val is_empty : t -> bool

  val add : elt -> t -> t

  val add_many : elt -> int -> t -> t

  val remove : elt -> t -> t

  val count : elt -> t -> int

  val map : (elt -> elt) -> t -> t

  val filter : (elt -> bool) -> t -> t

  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a

  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_with_counts : ('a -> elt * int -> 'a) -> 'a -> t -> 'a

  val merge : t -> t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val of_list : elt list -> t

  val to_list : t -> elt list

  val to_list_with_counts : t -> (elt * int) list

  val length : t -> int
end

module Make (E : Elt) : S with type elt = E.t = struct
  type elt = E.t

  type child = {key: E.component; subtree: t}

  and t = {count: int; children: child list}

  let empty = {count= 0; children= []}

  let neutral = empty

  let is_empty t = t.count = 0 && t.children = []

  let rec insert_child k sub = function
    | [] ->
        [{key= k; subtree= sub}]
    | c :: cs ->
        let cmp = E.compare_component c.key k in
        if cmp = 0 then {c with subtree= sub} :: cs
        else if cmp > 0 then {key= k; subtree= sub} :: c :: cs
        else c :: insert_child k sub cs

  let rec find_child k = function
    | [] ->
        None
    | c :: _ when E.compare_component c.key k = 0 ->
        Some c.subtree
    | _ :: cs ->
        find_child k cs

  let rec clean_children = function
    | [] ->
        []
    | c :: cs ->
        if is_empty c.subtree then clean_children cs else c :: clean_children cs

  let add_many elt n t =
    if n = 0 then t
    else
      let rec go path node =
        match path with
        | [] ->
            {node with count= max 0 (node.count + n)}
        | k :: ks ->
            let sub =
              Option.value (find_child k node.children) ~default:empty
            in
            let new_sub = go ks sub in
            let new_children =
              insert_child k new_sub node.children |> clean_children
            in
            {node with children= new_children}
      in
      go (E.split elt) t

  let add x t = add_many x 1 t

  let remove x t = add_many x (-1) t

  let count elt t =
    let rec go path node =
      match path with
      | [] ->
          node.count
      | k :: ks -> (
        match find_child k node.children with
        | None ->
            0
        | Some sub ->
            go ks sub )
    in
    go (E.split elt) t

  let rec merge t1 t2 =
    if t1 == t2 then t1
    else
      let new_count =
        let s = t1.count + t2.count in
        if s < 0 then 0 else s
      in
      let rec merge_ch l1 l2 =
        match (l1, l2) with
        | [], l | l, [] ->
            l
        | c1 :: cs1, c2 :: cs2 ->
            let cmp = E.compare_component c1.key c2.key in
            if cmp = 0 then
              {key= c1.key; subtree= merge c1.subtree c2.subtree}
              :: merge_ch cs1 cs2
            else if cmp < 0 then c1 :: merge_ch cs1 l2
            else c2 :: merge_ch l1 cs2
      in
      { count= new_count
      ; children= clean_children (merge_ch t1.children t2.children) }

  let rec equal t1 t2 = t1.count = t2.count && eq_ch t1.children t2.children

  and eq_ch l1 l2 =
    match (l1, l2) with
    | [], [] ->
        true
    | [], _ | _, [] ->
        false
    | c1 :: cs1, c2 :: cs2 ->
        E.compare_component c1.key c2.key = 0
        && equal c1.subtree c2.subtree
        && eq_ch cs1 cs2

  let rec compare t1 t2 =
    let c = Int.compare t1.count t2.count in
    if c <> 0 then c else cmp_ch t1.children t2.children

  and cmp_ch l1 l2 =
    match (l1, l2) with
    | [], [] ->
        0
    | [], _ ->
        -1
    | _, [] ->
        1
    | c1 :: cs1, c2 :: cs2 ->
        let k = E.compare_component c1.key c2.key in
        if k <> 0 then k
        else
          let s = compare c1.subtree c2.subtree in
          if s <> 0 then s else cmp_ch cs1 cs2

  let levels (t : t) : (elt * int) list list =
    let rec loop (curr : (E.component list * t) list)
        (acc : (elt * int) list list) =
      match curr with
      | [] ->
          List.rev acc
      | _ ->
          let layer_vals =
            List.filter_map
              (fun (pref_rev, node) ->
                if node.count > 0 then
                  Some (E.join (List.rev pref_rev), node.count)
                else None )
              curr
          in
          let next =
            List.fold_left
              (fun ns (pref_rev, node) ->
                List.fold_left
                  (fun ns {key; subtree} -> (key :: pref_rev, subtree) :: ns)
                  ns node.children )
              [] curr
            |> List.rev
          in
          loop next (layer_vals :: acc)
    in
    loop [([], t)] []

  let expand_layer_grouped (layer : (elt * int) list) : elt list =
    let rec rep acc x n = if n <= 0 then acc else rep (x :: acc) x (n - 1) in
    List.fold_left (fun acc (x, n) -> List.rev_append (rep [] x n) acc) [] layer
    |> List.rev

  let expand_layer_round_robin (layer : (elt * int) list) : elt list =
    let rec pass items acc =
      match items with
      | [] ->
          (List.rev acc, [])
      | (x, n) :: xs ->
          if n > 0 then
            let acc', rest = pass xs (x :: acc) in
            (acc', (x, n - 1) :: rest)
          else
            let acc', rest = pass xs acc in
            (acc', (x, 0) :: rest)
    in
    let rec loop items acc =
      if List.for_all (fun (_, n) -> n <= 0) items then List.rev acc
      else
        let emitted, items' = pass items [] in
        loop items' (List.rev_append emitted acc)
    in
    loop layer []

  let fold_left f acc t =
    let seq = levels t |> List.concat_map expand_layer_grouped in
    List.fold_left f acc seq

  let fold_right f t acc =
    let seq =
      levels t |> List.rev |> List.concat_map expand_layer_round_robin
    in
    List.fold_right f seq acc

  let fold_with_counts f acc t =
    levels t
    |> List.fold_left
         (fun a layer -> List.fold_left (fun a pair -> f a pair) a layer)
         acc

  let map g t =
    fold_with_counts (fun acc (x, n) -> add_many (g x) n acc) empty t

  let filter p t =
    fold_with_counts
      (fun acc (x, n) -> if p x then add_many x n acc else acc)
      empty t

  let of_list l = List.fold_left (fun acc x -> add x acc) empty l

  let to_list t = fold_right (fun x acc -> x :: acc) t []

  let to_list_with_counts t =
    fold_with_counts
      (fun acc (x, n) -> if n > 0 then (x, n) :: acc else acc)
      [] t
    |> List.rev

  let length t = fold_with_counts (fun acc (_, n) -> acc + n) 0 t
end
