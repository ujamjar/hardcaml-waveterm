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

type 'a wave = 
  | Clock 
  | Binary of 'a
  | Data of 'a

type 'a t = string * 'a wave

module Foo : sig

  module type W = sig
  
    include S

    type wave = 
      | Clock of string
      | Binary of string * t
      | Data of string * t * (elt -> string)

  end

  module Make(S : S) : W
    with type elt = S.elt
    and type t = S.t

end
