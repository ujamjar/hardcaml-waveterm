(*module type S = sig
  type elt
  type t
  val zero : elt 
  val one : elt
  val compare : elt -> elt -> bool
  val length : t -> int
  val get : t -> int -> elt
  val to_str : elt -> string
end

module Int : S 
  with type elt = int 
   and type t = int array

module Bits(B : HardCaml.Comb.S) : sig
  include S
  val make : unit -> t
  val set : t -> int -> elt -> unit
end
  with type elt = B.t
*)

(* element type *)
module type E = sig
  type elt
  val zero : elt 
  val one : elt
  val compare : elt -> elt -> bool
  val to_str : elt -> string
end

(* static (read only) buffer *)
module type S = sig
  include E
  type t
  val length : t -> int
  val get : t -> int -> elt
  val init : int -> (int -> elt) -> t
end

(* dynamic (growable) buffer *)
module type D = sig
  include S
  val make : unit -> t
  val set : t -> int -> elt -> unit
end

module Int : E with type elt = int
module Bits(B : HardCaml.Comb.S) : E with type elt = B.t

module Make_static(E : E) : S 
  with type elt = E.elt
   and type t = E.elt array
module Make_dynamic(E : E) : D 
  with type elt = E.elt

module type W = sig

  include S

  type wave = 
    | Clock of string
    | Binary of string * t
    | Data of string * t * (elt -> string)

  val get_name : wave -> string
  val get_data : wave -> t
  val get_to_str : wave -> (elt -> string)

end

module Make(S : S) : W
  with type elt = S.elt
  and type t = S.t


