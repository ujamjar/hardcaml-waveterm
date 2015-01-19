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

module Bits = struct
  module B = HardCaml.Bits.Comb.IntbitsList
  type elt = B.t
  type t = 
    {
      mutable data : elt array;
      mutable length : int;
    }
  let zero = B.gnd
  let one = B.vdd
  let compare a b = a = b
  let length d = d.length
  let get d n = Array.get d.data n
  let make () = { data = [||]; length = 0; }
  let resize d = 
    let old_data = d.data in
    let new_len = max 1 (Array.length d.data * 2) in
    d.data <- Array.init new_len (fun i -> try old_data.(i) with _ -> B.gnd)
  let rec set d n v = 
    try begin
      d.data.(n) <- v;
      d.length <- max d.length n
    end with _ -> begin
      resize d;
      set d n v
    end
  let to_str = B.to_bstr
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

end

module Make(S : S) = struct

  include S

  type wave = 
    | Clock of string
    | Binary of string * t
    | Data of string * t * (elt -> string)

  let get_name = function
    | Clock(n) -> n
    | Binary(n,_) -> n
    | Data(n,_,_) -> n

  let get_data = function
    | Clock(n) -> failwith "no clock data"
    | Binary(_,d) -> d
    | Data(_,d,_) -> d

  let get_to_str = function
    | Clock(n) -> failwith "no clock to_str"
    | Binary(_,_) -> failwith "no binary to_str"
    | Data(_,_,f) -> f

end

