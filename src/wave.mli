(* element type *)
module type E = sig
  type elt
  val zero : elt 
  val one : elt
  val compare : elt -> elt -> bool
  val to_bstr : elt -> string
  val to_sstr : elt -> string
  val to_ustr : elt -> string
  val to_hstr : elt -> string
  val to_int : elt -> int
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

  type to_str =
    | B (* binary *)
    | H (* hex *)
    | U (* unsigned int *)
    | S (* signed int *)
    | F of (elt -> string) (* function *)
    | I of string list (* index into strings *)

  type wave = 
    | Clock of string
    | Binary of string * t
    | Data of string * t * to_str

  val get_name : wave -> string
  val get_data : wave -> t
  val get_to_str : wave -> (elt -> string)

  type cfg = 
    {
      mutable wave_width : int; (** width of wave cycle *)
      mutable wave_height : int; (** height of wave cycle *)
      mutable wave_cycle : int; (** start cycle *)
    }

  val default : cfg

  type waves = 
    {
      cfg : cfg; (** render config *)
      waves : wave array; (** data *)
    }

  val write : out_channel -> waves -> unit
  val read : in_channel -> waves

end

module Make(E : E) : W
  with type elt = E.elt


