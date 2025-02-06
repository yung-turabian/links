val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c
val fst4 : 'a * 'b * 'c * 'd -> 'a
val snd4 : 'a * 'b * 'c * 'd -> 'b
val thd4 : 'a * 'b * 'c * 'd -> 'c
val for4 : 'a * 'b * 'c * 'd -> 'd
val fst5 : 'a * 'b * 'c * 'd * 'e -> 'a
val snd5 : 'a * 'b * 'c * 'd * 'e -> 'b
val thd5 : 'a * 'b * 'c * 'd * 'e -> 'c
val for5 : 'a * 'b * 'c * 'd * 'e -> 'd
val fth5 : 'a * 'b * 'c * 'd * 'e -> 'e

module Functional :
  sig
    val ( -<- ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    val ( ->- ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
    val identity : 'a -> 'a
    val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    val const : 'a -> 'b -> 'a
    val cross : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  end
val ( -<- ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( ->- ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val identity : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val const : 'a -> 'b -> 'a
val cross : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
val ( <| ) : ('a -> 'b) -> 'a -> 'b
val ( |> ) : 'a -> ('a -> 'b) -> 'b
module type OrderedShow =
  sig
    type t
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
  end
module type Map =
  sig
    type key
    type +!'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    exception Not_disjoint of key * string
    val filterv : ('a -> bool) -> 'a t -> 'a t
    val size : 'a t -> int
    val to_alist : 'a t -> (key * 'a) list
    val from_alist : (key * 'a) list -> 'a t
    val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
    val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t
    val pop : key -> 'a t -> 'a * 'a t
    val lookup : key -> 'a t -> 'a option
    val union_disjoint : 'a t -> 'a t -> 'a t
    val union_all : 'a t list -> 'a t
    val superimpose : 'a t -> 'a t -> 'a t
    val split_paired : ('a * 'b) t -> 'a t * 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
module String :
  sig
    type t = string
    val make : int -> char -> string
    val init : int -> (int -> char) -> string
    val empty : string
    external length : string -> int = "%string_length"
    external get : string -> int -> char = "%string_safe_get"
    val of_bytes : bytes -> string
    val to_bytes : string -> bytes
    val blit : string -> int -> bytes -> int -> int -> unit
    val concat : string -> string list -> string
    val cat : string -> string -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val starts_with : prefix:string -> string -> bool
    val ends_with : suffix:string -> string -> bool
    val contains_from : string -> int -> char -> bool
    val rcontains_from : string -> int -> char -> bool
    val contains : string -> char -> bool
    val sub : string -> int -> int -> string
    val split_on_char : char -> string -> string list
    val map : (char -> char) -> string -> string
    val mapi : (int -> char -> char) -> string -> string
    val fold_left : ('acc -> char -> 'acc) -> 'acc -> string -> 'acc
    val fold_right : (char -> 'acc -> 'acc) -> string -> 'acc -> 'acc
    val for_all : (char -> bool) -> string -> bool
    val exists : (char -> bool) -> string -> bool
    val trim : string -> string
    val escaped : string -> string
    val uppercase_ascii : string -> string
    val lowercase_ascii : string -> string
    val capitalize_ascii : string -> string
    val uncapitalize_ascii : string -> string
    val iter : (char -> unit) -> string -> unit
    val iteri : (int -> char -> unit) -> string -> unit
    val index_from_opt : string -> int -> char -> int option
    val rindex_from_opt : string -> int -> char -> int option
    val index_opt : string -> char -> int option
    val rindex_opt : string -> char -> int option
    val to_seq : t -> char Seq.t
    val to_seqi : t -> (int * char) Seq.t
    val of_seq : char Seq.t -> t
    val get_utf_8_uchar : t -> int -> Uchar.utf_decode
    val is_valid_utf_8 : t -> bool
    val get_utf_16be_uchar : t -> int -> Uchar.utf_decode
    val is_valid_utf_16be : t -> bool
    val get_utf_16le_uchar : t -> int -> Uchar.utf_decode
    val is_valid_utf_16le : t -> bool
    val get_uint8 : string -> int -> int
    val get_int8 : string -> int -> int
    val get_uint16_ne : string -> int -> int
    val get_uint16_be : string -> int -> int
    val get_uint16_le : string -> int -> int
    val get_int16_ne : string -> int -> int
    val get_int16_be : string -> int -> int
    val get_int16_le : string -> int -> int
    val get_int32_ne : string -> int -> int32
    val hash : t -> int
    val seeded_hash : int -> t -> int
    val get_int32_be : string -> int -> int32
    val get_int32_le : string -> int -> int32
    val get_int64_ne : string -> int -> int64
    val get_int64_be : string -> int -> int64
    val get_int64_le : string -> int -> int64
    external unsafe_get : string -> int -> char = "%string_unsafe_get"
    external unsafe_blit : string -> int -> bytes -> int -> int -> unit
      = "caml_blit_string" [@@noalloc]
    val index : string -> char -> int
    val rindex : string -> char -> int
    val index_from : string -> int -> char -> int
    val rindex_from : string -> int -> char -> int
    val pp : Format.formatter -> string -> unit
    val show : 'a -> 'a
    val blits : bytes -> int -> string -> int -> string list -> bytes
    val rev_concat : string -> string list -> string
  end
module Int :
  sig
    type t = int
    val compare : int -> int -> int
    val pp : Format.formatter -> int -> unit
    val show : int -> string
  end
module IntPair :
  sig
    type t = int * int
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : int * int -> int * int -> int
  end
module Char :
  sig
    external code : char -> int = "%identity"
    val chr : int -> char
    val escaped : char -> string
    val lowercase_ascii : char -> char
    val uppercase_ascii : char -> char
    type t = char
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val seeded_hash : int -> t -> int
    val hash : t -> int
    external unsafe_chr : int -> char = "%identity"
    val pp : Format.formatter -> char -> unit
    val show : char -> string
    val isAlpha : char -> bool
    val isAlnum : char -> bool
    val isWord : char -> bool
    val isLower : char -> bool
    val isUpper : char -> bool
    val isDigit : char -> bool
    val isXDigit : char -> bool
    val isBlank : char -> bool
  end
module Map :
  sig
    module type OrderedType = OrderedShow
    module type S = Map
    module Make :
      (Ord : OrderedType) ->
        sig
          type key = Ord.t
          type +!'a t
          val empty : 'a t
          val add : key -> 'a -> 'a t -> 'a t
          val add_to_list : key -> 'a -> 'a list t -> 'a list t
          val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val min_binding_opt : 'a t -> (key * 'a) option
          val max_binding : 'a t -> key * 'a
          val max_binding_opt : 'a t -> (key * 'a) option
          val choose : 'a t -> key * 'a
          val choose_opt : 'a t -> (key * 'a) option
          val find : key -> 'a t -> 'a
          val find_opt : key -> 'a t -> 'a option
          val find_first : (key -> bool) -> 'a t -> key * 'a
          val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val find_last : (key -> bool) -> 'a t -> key * 'a
          val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val of_list : (key * 'a) list -> 'a t
          val to_seq : 'a t -> (key * 'a) Seq.t
          val to_rev_seq : 'a t -> (key * 'a) Seq.t
          val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
          val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
          val of_seq : (key * 'a) Seq.t -> 'a t
          exception Not_disjoint of key * string
          val filterv : ('a -> bool) -> 'a t -> 'a t
          val size : 'a t -> int
          val to_alist : 'a t -> (key * 'a) list
          val from_alist : (key * 'a) list -> 'a t
          val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
          val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t
          val pop : key -> 'a t -> 'a * 'a t
          val lookup : key -> 'a t -> 'a option
          val union_disjoint : 'a t -> 'a t -> 'a t
          val union_all : 'a t list -> 'a t
          val superimpose : 'a t -> 'a t -> 'a t
          val split_paired : ('a * 'b) t -> 'a t * 'b t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
          val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
          val pp :
            (Format.formatter -> 'a -> unit) ->
            Format.formatter -> 'a t -> unit
        end
  end
module type Set =
  sig
    type elt
    type t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val union_all : t list -> t
    val from_list : elt list -> t
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
module Set :
  sig
    module type OrderedType = OrderedShow
    module type S = Set
    module Make :
      (Ord : OrderedType) ->
        sig
          type elt = Ord.t
          type t
          val empty : t
          val add : elt -> t -> t
          val singleton : elt -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val disjoint : t -> t -> bool
          val diff : t -> t -> t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val min_elt_opt : t -> elt option
          val max_elt : t -> elt
          val max_elt_opt : t -> elt option
          val choose : t -> elt
          val choose_opt : t -> elt option
          val find : elt -> t -> elt
          val find_opt : elt -> t -> elt option
          val find_first : (elt -> bool) -> t -> elt
          val find_first_opt : (elt -> bool) -> t -> elt option
          val find_last : (elt -> bool) -> t -> elt
          val find_last_opt : (elt -> bool) -> t -> elt option
          val iter : (elt -> unit) -> t -> unit
          val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
          val map : (elt -> elt) -> t -> t
          val filter : (elt -> bool) -> t -> t
          val filter_map : (elt -> elt option) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val split : elt -> t -> t * bool * t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val subset : t -> t -> bool
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val to_list : t -> elt list
          val of_list : elt list -> t
          val to_seq_from : elt -> t -> elt Seq.t
          val to_seq : t -> elt Seq.t
          val to_rev_seq : t -> elt Seq.t
          val add_seq : elt Seq.t -> t -> t
          val of_seq : elt Seq.t -> t
          val union_all : t list -> t
          val from_list : elt list -> t
          val pp : Format.formatter -> t -> unit
          val show : t -> string
        end
  end
module type INTSET =
  sig
    type elt = int
    type t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val union_all : t list -> t
    val from_list : elt list -> t
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
module IntSet :
  sig
    type elt = int
    type t = Set.Make(Int).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val union_all : t list -> t
    val from_list : elt list -> t
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
module IntMap :
  sig
    type key = int
    type 'a t = 'a Map.Make(Int).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    exception Not_disjoint of key * string
    val filterv : ('a -> bool) -> 'a t -> 'a t
    val size : 'a t -> int
    val to_alist : 'a t -> (key * 'a) list
    val from_alist : (key * 'a) list -> 'a t
    val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
    val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t
    val pop : key -> 'a t -> 'a * 'a t
    val lookup : key -> 'a t -> 'a option
    val union_disjoint : 'a t -> 'a t -> 'a t
    val union_all : 'a t list -> 'a t
    val superimpose : 'a t -> 'a t -> 'a t
    val split_paired : ('a * 'b) t -> 'a t * 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
module IntPairMap :
  sig
    type key = IntPair.t
    type 'a t = 'a Map.Make(IntPair).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    exception Not_disjoint of key * string
    val filterv : ('a -> bool) -> 'a t -> 'a t
    val size : 'a t -> int
    val to_alist : 'a t -> (key * 'a) list
    val from_alist : (key * 'a) list -> 'a t
    val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
    val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t
    val pop : key -> 'a t -> 'a * 'a t
    val lookup : key -> 'a t -> 'a option
    val union_disjoint : 'a t -> 'a t -> 'a t
    val union_all : 'a t list -> 'a t
    val superimpose : 'a t -> 'a t -> 'a t
    val split_paired : ('a * 'b) t -> 'a t * 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
module type STRINGMAP =
  sig
    type key = string
    type +!'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    exception Not_disjoint of key * string
    val filterv : ('a -> bool) -> 'a t -> 'a t
    val size : 'a t -> int
    val to_alist : 'a t -> (key * 'a) list
    val from_alist : (key * 'a) list -> 'a t
    val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
    val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t
    val pop : key -> 'a t -> 'a * 'a t
    val lookup : key -> 'a t -> 'a option
    val union_disjoint : 'a t -> 'a t -> 'a t
    val union_all : 'a t list -> 'a t
    val superimpose : 'a t -> 'a t -> 'a t
    val split_paired : ('a * 'b) t -> 'a t * 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
module type INTMAP =
  sig
    type key = int
    type +!'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    exception Not_disjoint of key * string
    val filterv : ('a -> bool) -> 'a t -> 'a t
    val size : 'a t -> int
    val to_alist : 'a t -> (key * 'a) list
    val from_alist : (key * 'a) list -> 'a t
    val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
    val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t
    val pop : key -> 'a t -> 'a * 'a t
    val lookup : key -> 'a t -> 'a option
    val union_disjoint : 'a t -> 'a t -> 'a t
    val union_all : 'a t list -> 'a t
    val superimpose : 'a t -> 'a t -> 'a t
    val split_paired : ('a * 'b) t -> 'a t * 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
module StringSet :
  sig
    type elt = string
    type t = Set.Make(String).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val union_all : t list -> t
    val from_list : elt list -> t
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
module StringMap : STRINGMAP
module type CHARSET =
  sig
    type elt = char
    type t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val union_all : t list -> t
    val from_list : elt list -> t
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
module CharSet : CHARSET
module CharMap :
  sig
    type key = char
    type 'a t = 'a Map.Make(Char).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    exception Not_disjoint of key * string
    val filterv : ('a -> bool) -> 'a t -> 'a t
    val size : 'a t -> int
    val to_alist : 'a t -> (key * 'a) list
    val from_alist : (key * 'a) list -> 'a t
    val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
    val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t
    val pop : key -> 'a t -> 'a * 'a t
    val lookup : key -> 'a t -> 'a option
    val union_disjoint : 'a t -> 'a t -> 'a t
    val union_all : 'a t list -> 'a t
    val superimpose : 'a t -> 'a t -> 'a t
    val split_paired : ('a * 'b) t -> 'a t * 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
type stringset = StringSet.t
val pp_stringset : Format.formatter -> stringset -> unit
val show_stringset : stringset -> string
type 'a stringmap = 'a StringMap.t
val pp_stringmap :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a stringmap -> unit
val show_stringmap :
  (Format.formatter -> 'a -> unit) -> 'a stringmap -> string
type intset = IntSet.t
val pp_intset : Format.formatter -> intset -> unit
val show_intset : intset -> string
type 'a intmap = 'a IntMap.t
val pp_intmap :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a intmap -> unit
val show_intmap : (Format.formatter -> 'a -> unit) -> 'a intmap -> string
val list_to_set : IntSet.elt list -> IntSet.t
module ListUtils :
  sig
    val empty : 'a list -> bool
    val fromTo : int -> int -> int list
    val mapIndex : ('a -> int -> 'b) -> 'a list -> 'b list
    val all_equiv : ('a -> 'a -> bool) -> 'a list -> bool
    val span : ('a -> bool) -> 'a list -> 'a list * 'a list
    val groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list
    val groupByPred : ('a -> 'b) -> 'a list -> 'a list list
    val groupByPred' : ('a -> 'b) -> 'a list -> 'a list list
    val unsnoc : 'a list -> 'a list * 'a
    val unsnoc_opt : 'a list -> ('a list * 'a) option
    val last : 'a list -> 'a
    val last_opt : 'a list -> 'a option
    val curtail : 'a list -> 'a list
    val difference : 'a list -> 'a list -> 'a list
    val remove_all : 'a list -> 'a list -> 'a list
    val subset : 'a list -> 'a list -> bool
    val less_to_cmp : ('a -> 'a -> bool) -> 'a -> 'a -> int
    val has_duplicates : 'a list -> bool
    val unduplicate : ('a -> 'a -> bool) -> 'a list -> 'a list
    val collect_duplicates : ('a -> 'a -> bool) -> 'a list -> 'a list
    val ordered_consecutive : int list -> bool
    val drop : int -> 'a list -> 'a list
    val take : int -> 'a list -> 'a list
    val split : int -> 'a list -> 'a list * 'a list
    val remove : 'a -> 'a list -> 'a list
    val concat_map : ('a -> 'b list) -> 'a list -> 'b list
    val concat_map_uniq : ('a -> 'b list) -> 'a list -> 'b list
    val concat_map_undup :
      ('a -> 'a -> bool) -> ('b -> 'a list) -> 'b list -> 'a list
    val for_each : 'a list -> ('a -> unit) -> unit
    val push_back : 'a -> 'a list ref -> unit
    val push_front : 'a -> 'a list ref -> unit
    val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
    val split4 :
      ('a * 'b * 'c * 'd) list -> 'a list * 'b list * 'c list * 'd list
    val drop_nth : 'a list -> int -> 'a list
    val filter_map : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
    exception Lists_length_mismatch
    val filter_map2 :
      ('a * 'b -> bool) -> ('a * 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val map_filter : ('a -> 'b) -> ('b -> bool) -> 'a list -> 'b list
    val print_list : string list -> string
    val zip : 'a list -> 'b list -> ('a * 'b) list
    val zip_with : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val split_with : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
    val zip' : 'a list -> 'b list -> ('a * 'b) list
    val zip_with' : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val transpose : 'a list list -> 'a list list
    val find_fstdiff : 'a list -> 'a option
  end
val empty : 'a list -> bool
val fromTo : int -> int -> int list
val mapIndex : ('a -> int -> 'b) -> 'a list -> 'b list
val all_equiv : ('a -> 'a -> bool) -> 'a list -> bool
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
val groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list
val groupByPred : ('a -> 'b) -> 'a list -> 'a list list
val groupByPred' : ('a -> 'b) -> 'a list -> 'a list list
val unsnoc : 'a list -> 'a list * 'a
val unsnoc_opt : 'a list -> ('a list * 'a) option
val last : 'a list -> 'a
val last_opt : 'a list -> 'a option
val curtail : 'a list -> 'a list
val difference : 'a list -> 'a list -> 'a list
val remove_all : 'a list -> 'a list -> 'a list
val subset : 'a list -> 'a list -> bool
val less_to_cmp : ('a -> 'a -> bool) -> 'a -> 'a -> int
val has_duplicates : 'a list -> bool
val unduplicate : ('a -> 'a -> bool) -> 'a list -> 'a list
val collect_duplicates : ('a -> 'a -> bool) -> 'a list -> 'a list
val ordered_consecutive : int list -> bool
val drop : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val split : int -> 'a list -> 'a list * 'a list
val remove : 'a -> 'a list -> 'a list
val concat_map : ('a -> 'b list) -> 'a list -> 'b list
val concat_map_uniq : ('a -> 'b list) -> 'a list -> 'b list
val concat_map_undup :
  ('a -> 'a -> bool) -> ('b -> 'a list) -> 'b list -> 'a list
val for_each : 'a list -> ('a -> unit) -> unit
val push_back : 'a -> 'a list ref -> unit
val push_front : 'a -> 'a list ref -> unit
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val split4 :
  ('a * 'b * 'c * 'd) list -> 'a list * 'b list * 'c list * 'd list
val drop_nth : 'a list -> int -> 'a list
val filter_map : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
exception Lists_length_mismatch
val filter_map2 :
  ('a * 'b -> bool) -> ('a * 'b -> 'c) -> 'a list -> 'b list -> 'c list
val map_filter : ('a -> 'b) -> ('b -> bool) -> 'a list -> 'b list
val print_list : string list -> string
val zip : 'a list -> 'b list -> ('a * 'b) list
val zip_with : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val split_with : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
val zip' : 'a list -> 'b list -> ('a * 'b) list
val zip_with' : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val transpose : 'a list list -> 'a list list
module AList :
  sig
    val rassoc_eq : ('b -> 'b -> bool) -> 'b -> ('a * 'b) list -> 'a
    val rassoc : 'a -> ('b * 'a) list -> 'b
    val rassq : 'a -> ('b * 'a) list -> 'b
    val rremove_assoc_eq :
      ('b -> 'b -> bool) -> 'b -> ('a * 'b) list -> ('a * 'b) list
    val rremove_assoc : 'a -> ('b * 'a) list -> ('b * 'a) list
    val rremove_assq : 'a -> ('b * 'a) list -> ('b * 'a) list
    val remove_keys : ('a * 'b) list -> 'a list -> ('a * 'b) list
    val alistmap : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
    val show_fgraph :
      ?glue:string -> (string -> string) -> string list -> string
    val alistmap' : ('a * 'b -> 'c) -> ('a * 'b) list -> ('a * 'c) list
    val map2alist : ('a -> 'b) -> 'a list -> ('a * 'b) list
    val graph_func : ('a -> 'b) -> 'a list -> ('a * 'b) list
    val range : ('a * 'b) list -> 'b list
    val dom : ('a * 'b) list -> 'a list
    val lookup_in : ('a * 'b) list -> 'a -> 'b
    val lookup : 'a -> ('a * 'b) list -> 'b option
  end
val rassoc_eq : ('b -> 'b -> bool) -> 'b -> ('a * 'b) list -> 'a
val rassoc : 'a -> ('b * 'a) list -> 'b
val rassq : 'a -> ('b * 'a) list -> 'b
val rremove_assoc_eq :
  ('b -> 'b -> bool) -> 'b -> ('a * 'b) list -> ('a * 'b) list
val rremove_assoc : 'a -> ('b * 'a) list -> ('b * 'a) list
val rremove_assq : 'a -> ('b * 'a) list -> ('b * 'a) list
val remove_keys : ('a * 'b) list -> 'a list -> ('a * 'b) list
val alistmap : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
val show_fgraph : ?glue:string -> (string -> string) -> string list -> string
val alistmap' : ('a * 'b -> 'c) -> ('a * 'b) list -> ('a * 'c) list
val map2alist : ('a -> 'b) -> 'a list -> ('a * 'b) list
val graph_func : ('a -> 'b) -> 'a list -> ('a * 'b) list
val range : ('a * 'b) list -> 'b list
val dom : ('a * 'b) list -> 'a list
val lookup_in : ('a * 'b) list -> 'a -> 'b
val lookup : 'a -> ('a * 'b) list -> 'b option
module StringUtils :
  sig
    val string_of_char : char -> string
    val string_of_alist : (string * string) list -> string
    val split_string : string -> char -> string list
    val split : char -> string -> string list
    val explode : string -> char list
    val is_numeric : string -> bool
    val implode : char list -> string
    val contains : (char -> bool) -> string -> bool
    val find_char : bytes -> char -> int list
    val mapstrcat : string -> ('a -> string) -> 'a list -> string
    val string_starts_with : string -> string -> bool
    val start_of : is:string -> string -> bool
    val end_of : is:string -> string -> bool
    val count : char -> string -> int
    val replace : string -> string -> string -> string
  end
val string_of_char : char -> string
val string_of_alist : (string * string) list -> string
val split_string : string -> char -> string list
val split : char -> string -> string list
val explode : string -> char list
val is_numeric : string -> bool
val implode : char list -> string
val contains : (char -> bool) -> string -> bool
val find_char : bytes -> char -> int list
val mapstrcat : string -> ('a -> string) -> 'a list -> string
val string_starts_with : string -> string -> bool
val start_of : is:string -> string -> bool
val end_of : is:string -> string -> bool
val count : char -> string -> int
val replace : string -> string -> string -> string
val groupingsToString : ('a -> string) -> 'a list list -> string
val numberp : string -> bool
val lines : in_channel -> string list
val call_with_open_infile :
  string -> ?binary:bool -> (in_channel -> 'a) -> 'a
val call_with_open_outfile :
  string -> ?binary:bool -> (out_channel -> 'a) -> 'a
val process_output : string -> string
val filter_through : command:string -> string -> string
val newer : string -> string -> bool
val absolute_path : string -> string
val getuid_owns : string -> bool
val mem_assoc3 : 'a -> ('a * 'b * 'c) list -> bool
type ('a, 'b) either = Left of 'a | Right of 'b
val pp_either :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) either -> unit
val show_either :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) -> ('a, 'b) either -> string
val inLeft : 'a -> ('a, 'b) either
val inRight : 'a -> ('b, 'a) either
val fromLeft : ('a, 'b) either -> 'a
val fromRight : ('a, 'b) either -> 'b
val either_partition :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
module Queue :
  sig
    type 'a t = 'a Queue.t
    exception Empty
    val create : unit -> 'a t
    val add : 'a -> 'a t -> unit
    val push : 'a -> 'a t -> unit
    val take : 'a t -> 'a
    val take_opt : 'a t -> 'a option
    val pop : 'a t -> 'a
    val peek : 'a t -> 'a
    val peek_opt : 'a t -> 'a option
    val top : 'a t -> 'a
    val drop : 'a t -> unit
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val iter : ('a -> unit) -> 'a t -> unit
    val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
    val transfer : 'a t -> 'a t -> unit
    val to_seq : 'a t -> 'a Seq.t
    val add_seq : 'a t -> 'a Seq.t -> unit
    val of_seq : 'a Seq.t -> 'a t
    val to_list : 'a Queue.t -> 'a list
    val of_list : 'a list -> 'a Queue.t
  end
module OptionUtils :
  sig
    exception EmptyOption
    val val_of : 'a option -> 'a
    val is_some : 'a option -> bool
    val opt_app : ('a -> 'b) -> 'b -> 'a option -> 'b
    val opt_map : ('a -> 'b) -> 'a option -> 'b option
    val opt_bind : ('a -> 'b option) -> 'a option -> 'b option
    val opt_split : ('a * 'b) option -> 'a option * 'b option
    val opt_iter : ('a -> 'b) -> 'a option -> unit
    val from_option : 'a -> 'a option -> 'a
    val perhaps_apply : ('a -> 'a option) -> 'a -> 'a
    val opt_as_list : 'a option -> 'a list
    val opt_sequence : 'a option list -> 'a list option
    val some : 'a -> 'a option
    val ( >>=? ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( ||=? ) : 'a option -> 'a option -> 'a option
    val ( >>==? ) : 'a list -> ('a -> 'a option) -> 'a list option
    val map_tryPick :
      (string -> 'a -> 'b option) -> 'a stringmap -> 'b option
    val list_tryPick : ('a -> 'b option) -> 'a list -> 'b option
  end
exception EmptyOption
val val_of : 'a option -> 'a
val is_some : 'a option -> bool
val opt_app : ('a -> 'b) -> 'b -> 'a option -> 'b
val opt_map : ('a -> 'b) -> 'a option -> 'b option
val opt_bind : ('a -> 'b option) -> 'a option -> 'b option
val opt_split : ('a * 'b) option -> 'a option * 'b option
val opt_iter : ('a -> 'b) -> 'a option -> unit
val from_option : 'a -> 'a option -> 'a
val perhaps_apply : ('a -> 'a option) -> 'a -> 'a
val opt_as_list : 'a option -> 'a list
val opt_sequence : 'a option list -> 'a list option
val some : 'a -> 'a option
val ( >>=? ) : 'a option -> ('a -> 'b option) -> 'b option
val ( ||=? ) : 'a option -> 'a option -> 'a option
val ( >>==? ) : 'a list -> ('a -> 'a option) -> 'a list option
val map_tryPick :
  (StringMap.key -> 'a -> 'b option) -> 'a StringMap.t -> 'b option
val list_tryPick : ('a -> 'b option) -> 'a list -> 'b option
val read_octal : string -> char
val read_hex : string -> char
val escape_regexp : Str.regexp
val decode_escapes : string -> string
val xml_escape : string -> string
val xml_unescape : string -> string
val base64decode : string -> string
val base64encode : string -> string
val gensym_counter : int ref
val gensym : ?prefix:string -> unit -> string
val pair_fresh_names : ?prefix:string -> 'a list -> ('a * string) list
val refresh_names : string list -> (string * string) list
val any_true : bool list -> bool
val getenv : string -> string option
val safe_getenv : string -> string
module Buffer = Notfound.Buffer
module Hashtbl = Notfound.Hashtbl
module List = Notfound.List
module ListLabels = Notfound.ListLabels
module MoreLabels = Notfound.MoreLabels
module Str = Notfound.Str
module StringLabels = Notfound.StringLabels
module Sys = Notfound.Sys
module Unix = Notfound.Unix
module UnixLabels = Notfound.UnixLabels
module Printexc = Notfound.Printexc
exception NotFound of string
val pow : int -> int -> int
val string_of_float' : float -> string
val time_seconds : unit -> int
val time_milliseconds : unit -> int
val time_microseconds : unit -> int
val strip_leading_slash : string -> string
val strip_trailing_slash : string -> string
val strip_slashes : string -> string
val format_omission : Format.formatter -> unit
module Disk :
  sig
    exception End_of_stream
    exception AccessError of string
    exception BadLink
    type dir_t
    type file_t
    type link_t
    type item = Directory of dir_t | File of file_t | Link of link_t
    type inode = { no : int; data : item; }
    module Directory :
      sig
        type t = dir_t
        val basename : t -> string
        val dirname : t -> string
        val to_filename : t -> string
        val of_path : string -> t
      end
    module File :
      sig
        type t = file_t
        val basename : t -> string
        val dirname : t -> string
        val relative_name : t -> string
        val to_filename : t -> string
      end
    module Link : sig type t = link_t val follow : t -> inode end
    module Iterator :
      sig
        type t
        val next : t -> inode
        val finalise : t -> unit
        val of_directory : dir_t -> t
      end
  end
module type GLOB_POLICY =
  sig
    val symlinks : [ `AlwaysFollow | `FollowOnce | `FollowOnceQuietly ]
    val scan_depth : [ `Bounded of int | `SystemDefault ]
  end
module DefaultPolicy : GLOB_POLICY
module Glob :
  sig
    module type S =
      sig
        exception CyclicLinkError
        val files : string -> Str.regexp -> Disk.file_t list
        val files_ext : string -> string list -> Disk.file_t list
      end
    module Make :
      (P : GLOB_POLICY) ->
        sig
          exception CyclicLinkError
          val files : string -> Str.regexp -> Disk.file_t list
          val files_ext : string -> string list -> Disk.file_t list
        end
  end
val locate_file : string -> string
module LwtHelpers :
  sig
    val foldl_lwt : ('a -> 'b -> 'a Lwt.t) -> 'a Lwt.t -> 'b list -> 'a Lwt.t
    val foldr_lwt : ('a -> 'b -> 'b Lwt.t) -> 'a list -> 'b Lwt.t -> 'b Lwt.t
    val sequence : 'a Lwt.t list -> 'a list Lwt.t
  end
module PolyBuffer :
  sig
    type 'a buf
    val init : int -> int -> 'a -> 'a buf
    val length : 'a buf -> int
    val get : 'a buf -> int -> 'a
    val set : 'a buf -> int -> 'a -> unit
    val append : 'a buf -> 'a -> unit
    val to_list : 'a buf -> 'a list
  end
module CalendarShow :
  sig
    module Date = CalendarLib.Fcalendar.Precise.Date
    module Time = CalendarLib.Fcalendar.Precise.Time
    type t = CalendarLib.Fcalendar.Precise.t
    type day = Date.day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    type month =
      Date.month =
        Jan
      | Feb
      | Mar
      | Apr
      | May
      | Jun
      | Jul
      | Aug
      | Sep
      | Oct
      | Nov
      | Dec
    type year = int
    type second = float
    type field =
        [ `Day | `Hour | `Minute | `Month | `Second | `Week | `Year ]
    val make : int -> int -> int -> int -> int -> second -> t
    val lmake :
      year:int ->
      ?month:int ->
      ?day:int -> ?hour:int -> ?minute:int -> ?second:second -> unit -> t
    val create : Date.t -> Time.t -> t
    val now : unit -> t
    val from_jd : float -> t
    val from_mjd : float -> t
    val convert :
      t -> CalendarLib.Time_Zone.t -> CalendarLib.Time_Zone.t -> t
    val to_gmt : t -> t
    val from_gmt : t -> t
    val days_in_month : t -> int
    val day_of_week : t -> day
    val day_of_month : t -> int
    val day_of_year : t -> int
    val week : t -> int
    val month : t -> month
    val year : t -> int
    val to_jd : t -> float
    val to_mjd : t -> float
    val hour : t -> int
    val minute : t -> int
    val second : t -> second
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val is_leap_day : t -> bool
    val is_gregorian : t -> bool
    val is_julian : t -> bool
    val is_pm : t -> bool
    val is_am : t -> bool
    val to_unixtm : t -> Unix.tm
    val from_unixtm : Unix.tm -> t
    val to_unixfloat : t -> float
    val from_unixfloat : float -> t
    val from_date : Date.t -> t
    val to_date : t -> Date.t
    val to_time : t -> Time.t
    module Period = CalendarLib.Fcalendar.Precise.Period
    val add : t -> [< CalendarLib.Period.date_field ] Period.period -> t
    val sub :
      t -> t -> [< CalendarLib.Period.date_field > `Day `Week ] Period.period
    val precise_sub : t -> t -> Period.t
    val rem : t -> [< CalendarLib.Period.date_field ] Period.period -> t
    val next : t -> field -> t
    val prev : t -> field -> t
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
module UnixTimestamp :
  sig
    val of_calendar : CalendarShow.t -> float
    val to_calendar : Unix.tm -> CalendarShow.t
    val to_local_calendar : float -> CalendarShow.t
    val to_utc_calendar : float -> CalendarShow.t
  end
module IO :
  sig
    module Channel : sig val cat : in_channel -> out_channel -> unit end
  end
