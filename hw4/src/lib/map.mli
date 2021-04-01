(* Helper Module for Map of Core package *)

exception Error of string

(******************************************************************************)
(******************************************************************************)
(* Map                                                                        *)
(******************************************************************************)
(******************************************************************************)

module CPMap = Core.Map.Poly
(******************************************************************************* 
  In this module, we only use partial functionalities of Core.Map.Poly module.
  If you are an expert in using Core.Map.Poly module, you can use them.
*******************************************************************************)

type ('a, 'b) t = ('a, 'b) CPMap.t

(* The empty map. *)
val empty : ('a, 'b) t

(* Map with one (key, data) pair. *)
val singleton : 'a -> 'b -> ('a, 'b) t

(* Creates map from an association list with unique keys.
   If there is any duplicated key, an error will be raised. *)
val of_alist : ('a * 'b) list -> ('a, 'b) t

(* Tests whether a map is empty or not. *)
val is_empty : ('a, 'b) t -> bool

(* `length m` returns number of elements in `m`. *)
val length : ('a, 'b) t -> int

(* `add m ~key:k ~data:d` returns `m` extended with `k` mapped to `d`.
   If `k` is already member in `m`, an error will be raised. *)
val add : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t

(* `set m ~key:k ~data:d` returns a new map `m'` that is the same as `m` on all
   keys except for `k`, and whose value for `k` is mapped to `d`. *)
val set : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t

(* `change m k ~f` returns a new map `m'` that is the same as `m` on all keys
   except for `k`, and whose value for `k` is defined by `f`.
   i.e., `find m' k = f (find m k)`. *)
val change : ('a, 'b) t -> 'a -> f:('b option -> 'b option) -> ('a, 'b) t

(* `update m k ~f` is same with `change m k ~f:(fun o -> Some (f o))` *)
val update : ('a, 'b) t -> 'a -> f:('b option -> 'b) -> ('a, 'b) t

(* Returns the value bound to the given key if it exists,
   and `None` otherwise *)
val find : ('a, 'b) t -> 'a -> 'b option

(* Returns a new map with any binding for the key in question removed. *)
val remove : ('a, 'b) t -> 'a -> ('a, 'b) t

(* `mem m k` tests whether `m` contains a binding for `k`. *)
val mem : ('a, 'b) t -> 'a -> bool

(* Iterates data in a map. *)
val iter : ('a, 'b) t -> f:('b -> unit) -> unit

(* Iterates keys in a map. *)
val iterk : ('a, 'b) t -> f:('a -> unit) -> unit

(* Iterates all pairs in a map. *)
val iteri : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit

(* Iterates two maps side by side. *)
val iter2 : ('a, 'b1) t -> ('a, 'b2) t 
            -> f:(key:'a 
                  -> data:[`Left of 'b1 | `Right of 'b2 | `Both of ('b1 * 'b2)]
                  -> unit)
            -> unit

(* Returns new map with bound values replaced by the result of `f` applied to 
   them *)
val map : ('a, 'b1) t -> f:('b1 -> 'b2) -> ('a, 'b2) t

(* Returns new map with bound values replaced by the result of `f` applied to 
   them *)
val mapi : ('a, 'b1) t -> f:(key:'a -> data:'b1 -> 'b2) -> ('a, 'b2) t

(* Folds over keys and data in map in increasing order of key *)
val fold : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

(* Folds over keys and data in map in decreasing order of key *)
val fold_right : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) 
                  -> 'c

(* Folds two maps side by side. *)
val fold2 : ('a, 'b1) t -> ('a, 'b2) t
            -> init:'c
            -> f:(key:'a 
                  -> data:[`Left of 'b1 | `Right of 'b2 | `Both of ('b1 * 'b2)]
                  -> 'c
                  -> 'c) 
            -> 'c

(* Accumulate each data retained by f into a new map *)
val filter : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t

(* Accumulate each key retained by f into a new map *)
val filterk : ('a, 'b) t -> f:('a -> bool) -> ('a, 'b) t

(* Accumulate each key and data retained by f into a new map *)
val filteri : ('a, 'b) t -> f:(key:'a -> data:'b -> bool) -> ('a, 'b) t

(* Returns new map with bound values filtered by the result of `f` applied to 
   them. *)
val filter_map : ('a, 'b1) t -> f:('b1 -> 'b2 option) -> ('a, 'b2) t

(* Returns new map with bound values filtered by the result of `f` applied to 
   them. *)
val filter_mapi : ('a, 'b1) t 
                  -> f:(key:'a -> data:'b1 -> 'b2 option) -> ('a, 'b2) t

(* `equal cmp m1 m2` tests whether the maps m1 and m2 are equal, that is,
   contain equal keys and associate them with equal data.
   `cmp` is the equality predicate used to compare the data associated with the
   keys. *)
val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

(* Returns list of keys in map. *)
val keys : ('a, 'b) t -> 'a list

(* Returns list of data in map. *)
val data : ('a, 'b) t -> 'b list

(* Creates association list from map.
   Parameter key_order default is `Increasing *)
val to_alist : ?key_order:[`Increasing | `Decreasing] -> ('a, 'b) t
                -> ('a * 'b) list
