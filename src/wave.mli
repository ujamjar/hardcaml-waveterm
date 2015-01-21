(* element type *)
module type E = sig
  type elt
  val zero : elt 
  val one : elt
  val compare : elt -> elt -> bool
  val to_str : elt -> string
end

module Bits(B : HardCaml.Comb.S) : E with type elt = B.t

module type S = sig
  include E
  type t
  val length : t -> int
  val get : t -> int -> elt
  val init : int -> (int -> elt) -> t
  val make : unit -> t
  val set : t -> int -> elt -> unit
end

module type W = sig

  include S

  type wave = 
    | Clock of string
    | Binary of string * t
    | Data of string * t * (elt -> string)

  val get_name : wave -> string
  val get_data : wave -> t
  val get_to_str : wave -> (elt -> string)

  type waves = 
    {
      mutable wave_width : int; (** width of wave cycle *)
      mutable wave_height : int; (** height of wave cycle *)
      mutable wave_cycle : int; (** start cycle *)
      waves : wave array; (** data *)
    }

end

module Make(E : E) : W
  with type elt = E.elt


