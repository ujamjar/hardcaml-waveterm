module type S = sig
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

module Bits : sig
  include S
  val make : unit -> t
  val set : t -> int -> elt -> unit
end
  with type elt = HardCaml.Bits.Comb.IntbitsList.t

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

