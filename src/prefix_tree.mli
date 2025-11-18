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

module Make (E : Elt) : S with type elt = E.t
