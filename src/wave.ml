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

module Int = struct
  type elt = int
  let zero = 0 
  let one = 1
  let compare a b = a = b
  let to_str = string_of_int
end

module Bits(B : HardCaml.Comb.S) = struct
  type elt = B.t
  let zero = B.gnd
  let one = B.vdd
  let compare a b = a = b
  let to_str = B.to_bstr
end

module Make_static(E : E) = struct
  include E
  type t = elt array
  let length = Array.length
  let get = Array.get
  let init = Array.init
end

module Make_dynamic(E : E) = struct
  include E
  type t = 
    {
      mutable data : elt array;
      mutable length : int;
    }
  
  let length d = d.length
  
  let get d n = 
    if n < d.length then
      Array.get d.data n
    else
      raise (Invalid_argument "wave out of bounds")
  
  let make () = 
    { 
      data = [||]; 
      length = 0; 
    }
  
  let init n f = 
    {
      data = Array.init n f;
      length = n;
    }
  
  let resize d = 
    let old_data = d.data in
    let new_len = max 1 (Array.length d.data * 2) in
    d.data <- Array.init new_len (fun i -> try old_data.(i) with _ -> zero)
  
  let rec set d n v = 
    try begin
      Array.set d.data n v;
      d.length <- max d.length (n+1)
    end with _ -> begin
      resize d;
      set d n v
    end

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

