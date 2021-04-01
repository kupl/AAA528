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
let empty : ('a, 'b) t
= CPMap.empty

(* Map with one (key, data) pair. *)
let singleton : 'a -> 'b -> ('a, 'b) t
= CPMap.singleton

(* Creates map from an association list with unique keys.
   If there is any duplicated key, an error will be raised. *)
let of_alist : ('a * 'b) list -> ('a, 'b) t
= fun l -> begin
  (* of_alist function start *)
  l
  |> CPMap.of_alist
  |> function 
      | `Ok mm -> mm
      | `Duplicate_key _ -> Error "of_alist : duplicated key in alist"
                            |> Stdlib.raise
  (* of_alist function end *)
end

(* Tests whether a map is empty or not. *)
let is_empty : ('a, 'b) t -> bool
= CPMap.is_empty

(* `length m` returns number of elements in `m`. *)
let length : ('a, 'b) t -> int
= CPMap.length

(* `add m ~key:k ~data:d` returns `m` extended with `k` mapped to `d`.
   If `k` is already member in `m`, an error will be raised. *)
let add : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t
= fun m ~key ~data -> begin
  (* add function start *)
  m
  |> CPMap.add ~key ~data
  |> function
      | `Ok mm -> mm
      | `Duplicate -> Error "add : duplicated key in map" |> Stdlib.raise
  (* add function end *)
end

(* `set m ~key:k ~data:d` returns a new map `m'` that is the same as `m` on all
   keys except for `k`, and whose value for `k` is mapped to `d`. *)
let set : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t
= CPMap.set

(* `change m k ~f` returns a new map `m'` that is the same as `m` on all keys
   except for `k`, and whose value for `k` is defined by `f`.
   i.e., `find m' k = f (find m k)`. *)
let change : ('a, 'b) t -> 'a -> f:('b option -> 'b option) -> ('a, 'b) t
= CPMap.change

(* `update m k ~f` is same with `change m k ~f:(fun o -> Some (f o))` *)
let update : ('a, 'b) t -> 'a -> f:('b option -> 'b) -> ('a, 'b) t
= CPMap.update

(* Returns the value bound to the given key if it exists,
   and `None` otherwise *)
let find : ('a, 'b) t -> 'a -> 'b option
= CPMap.find

(* Returns a new map with any binding for the key in question removed. *)
let remove : ('a, 'b) t -> 'a -> ('a, 'b) t
= CPMap.remove

(* `mem m k` tests whether `m` contains a binding for `k`. *)
let mem : ('a, 'b) t -> 'a -> bool
= CPMap.mem

(* Iterates data in a map. *)
let iter : ('a, 'b) t -> f:('b -> unit) -> unit
= CPMap.iter

(* Iterates keys in a map. *)
let iterk : ('a, 'b) t -> f:('a -> unit) -> unit
= CPMap.iter_keys

(* Iterates all pairs in a map. *)
let iteri : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
= CPMap.iteri

(* Iterates two maps side by side. *)
let iter2 : ('a, 'b1) t -> ('a, 'b2) t 
            -> f:(key:'a 
                  -> data:[`Left of 'b1 | `Right of 'b2 | `Both of ('b1 * 'b2)]
                  -> unit)
            -> unit
= CPMap.iter2

(* Returns new map with bound values replaced by the result of `f` applied to 
   them *)
let map : ('a, 'b1) t -> f:('b1 -> 'b2) -> ('a, 'b2) t
= CPMap.map

(* Returns new map with bound values replaced by the result of `f` applied to 
   them *)
let mapi : ('a, 'b1) t -> f:(key:'a -> data:'b1 -> 'b2) -> ('a, 'b2) t
= CPMap.mapi

(* Folds over keys and data in map in increasing order of key *)
let fold : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
= CPMap.fold

(* Folds over keys and data in map in decreasing order of key *)
let fold_right : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) 
                  -> 'c
= CPMap.fold_right

(* Folds two maps side by side. *)
let fold2 : ('a, 'b1) t -> ('a, 'b2) t
            -> init:'c
            -> f:(key:'a 
                  -> data:[`Left of 'b1 | `Right of 'b2 | `Both of ('b1 * 'b2)]
                  -> 'c
                  -> 'c) 
            -> 'c
= CPMap.fold2

(* Accumulate each data retained by f into a new map *)
let filter : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t
= CPMap.filter

(* Accumulate each key retained by f into a new map *)
let filterk : ('a, 'b) t -> f:('a -> bool) -> ('a, 'b) t
= CPMap.filter_keys

(* Accumulate each key and data retained by f into a new map *)
let filteri : ('a, 'b) t -> f:(key:'a -> data:'b -> bool) -> ('a, 'b) t
= CPMap.filteri

(* Returns new map with bound values filtered by the result of `f` applied to 
   them. *)
let filter_map : ('a, 'b1) t -> f:('b1 -> 'b2 option) -> ('a, 'b2) t
= CPMap.filter_map

(* Returns new map with bound values filtered by the result of `f` applied to 
   them. *)
let filter_mapi : ('a, 'b1) t 
                  -> f:(key:'a -> data:'b1 -> 'b2 option) -> ('a, 'b2) t
= CPMap.filter_mapi

(* `equal cmp m1 m2` tests whether the maps m1 and m2 are equal, that is,
   contain equal keys and associate them with equal data.
   `cmp` is the equality predicate used to compare the data associated with the
   keys. *)
let equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
= CPMap.equal

(* Returns list of keys in map. *)
let keys : ('a, 'b) t -> 'a list
= CPMap.keys

(* Returns list of data in map. *)
let data : ('a, 'b) t -> 'b list
= CPMap.data

(* Creates association list from map.
   Parameter key_order default is `Increasing *)
let to_alist : ?key_order:[`Increasing | `Decreasing] -> ('a, 'b) t
                -> ('a * 'b) list
= CPMap.to_alist
