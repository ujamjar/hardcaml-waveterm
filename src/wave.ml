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

module Int = struct
  type elt = int
  type t = int array
  let zero = 0 
  let one = 1
  let compare a b = a = b
  let length = Array.length
  let get = Array.get
  let to_str = string_of_int
end

type 'a wave = 
  | Clock 
  | Binary of 'a
  | Data of 'a

type 'a t = string * 'a wave

module Foo = struct

  module type W = sig
  
    include S

    type wave = 
      | Clock of string
      | Binary of string * t
      | Data of string * t * (elt -> string)

  end

  module Make(S : S) = struct

    include S

    type wave = 
      | Clock of string
      | Binary of string * t
      | Data of string * t * (elt -> string)

  end

end

