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

module type S = sig
  include E
  type t
  val length : t -> int
  val get : t -> int -> elt
  val init : int -> (int -> elt) -> t
  val make : unit -> t
  val set : t -> int -> elt -> unit
end

module Bits(B : HardCaml.Comb.S) = struct
  type elt = B.t
  let zero = B.gnd
  let one = B.vdd
  let compare a b = a = b

  (* string conversions *)
  let to_bstr = B.to_bstr

  let rec to_hstr b = 
    let to_char i = 
      Char.chr (if i < 10 then Char.code '0' + i else Char.code 'A' + i - 10)
    in
    let blen = B.width b in
    let slen = (blen + 3) / 4 in
    Bytes.init slen (fun i ->
      let i = slen - i - 1 in
      let l = i*4 in
      let h = (min blen (l+4)) - 1 in
      to_char (B.to_int (B.select b h l)))

  (* convert to integer using arbitrary precision. *)
  let to_ustr b = 
    let max = 29 in (* safe max positive int bits *)
    if B.width b <= max then string_of_int (B.to_int b)
    else
      (* convert with big ints *)
      let rec f b acc =
        let (+:) = Big_int.add_big_int in
        let (<<:) = Big_int.shift_left_big_int in
        let to_big b = Big_int.big_int_of_int (B.to_int b) in
        if B.width b <= max then
          (* result *)
          (acc <<: (B.width b)) +: to_big b
        else 
          let t, b = B.sel_top b max, B.drop_top b max in
          f b ((acc <<: max) +: to_big t)
      in
      Big_int.(string_of_big_int (f b zero_big_int))

  (* signed conversion uses unsigned conversion with detection of sign *)
  let to_sstr b = 
    let max = 29 in (* safe max positive int bits *)
    if B.width b <= max then string_of_int (B.to_sint b)
    else 
      if B.to_int (B.msb b) = 0 then to_ustr b
      else
        "-" ^ (to_ustr B.((~: b) +:. 1)) (* conv -ve to +ve, leading '-' *)

  let to_int = B.to_int
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
      mutable wave_width : int; 
      mutable wave_height : int; 
      mutable start_cycle : int; 
      mutable start_signal : int; 
      mutable wave_cursor : int;
      mutable signal_cursor : int;
      mutable signal_scroll : int;
      mutable value_scroll : int;
    }

  val default : cfg

  type waves = 
    {
      cfg : cfg;
      waves : wave array; 
    }

  val write : out_channel -> waves -> unit
  val read : in_channel -> waves

end

module Make(E : E) = struct

  module D = Make_dynamic(E)
  include D

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
    | Data(_,_,f) -> begin
      match f with
      | B -> to_bstr
      | H -> to_hstr
      | U -> to_ustr
      | S -> to_sstr
      | F f -> f
      | I s -> (fun elt -> try List.nth s (to_int elt) with _ -> "-")
    end

  type cfg = 
    {
      mutable wave_width : int; 
      mutable wave_height : int; 
      mutable start_cycle : int; 
      mutable start_signal : int; 
      mutable wave_cursor : int;
      mutable signal_cursor : int;
      mutable signal_scroll : int;
      mutable value_scroll : int;
    }

  let default = 
    {
      wave_width = 3;
      wave_height = 1;
      start_cycle = 0;
      start_signal = 0;
      wave_cursor = -1;
      signal_cursor = -1;
      signal_scroll = 0;
      value_scroll = 0;
    }
      
  type waves = 
    {
      cfg : cfg;
      waves : wave array; 
    }

  let write ch w = 
    let w = 
      { w with waves = Array.map
        (function 
          | Clock(n) -> Clock(n)
          | Binary(n, d) -> 
            Binary(n, { d with data=Array.init d.length (Array.get d.data); })
          | Data(n, d, ts) -> 
            let ts = match ts with F _ -> B | _ -> ts in (* cant marshal functions *)
            Data(n, { d with data=Array.init d.length (Array.get d.data); }, ts)) 
        w.waves
      }
    in
    Marshal.to_channel ch w []

  let read ch = (Marshal.from_channel ch : waves)
(*
  let rle d = 
    List.rev @@ Array.fold_left 
      (fun acc x ->
        match acc with
        | [] -> [1,x]
        | (n,y)::t -> 
          if E.compare x y then ((n+1,y)::t) else (1,x)::(n,y)::t)
      [] d

  (* XXX need a way to pack the elements *)

  let write_chunk ch w ~ofs ~size = 
    (* extract the chunk *)
    let w = 
      let init d = Array.init size (fun j -> Array.get d (j+ofs)) in
      { w with waves = Array.map 
        (function
          | Clock(n) -> Clock(n)
          | Binary(n, d) ->
            Binary(n, { d with data=init d.data; })
          | Data(n, d, ts) -> 
            let ts = match ts with F _ -> B | _ -> ts in (* cant marshal functions *)
            Data(n, { d with data=init d.data; }, ts)) 
          w.waves
        }
    in 
    (* now compress the chunk *)
    w
*)
end




