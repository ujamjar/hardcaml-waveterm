module Style : sig
  type colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  type t = 
    {
      bold : bool;
      fg : colour;
      bg : colour;
    }
  val default : t
end

type rect = 
  {
    r : int;
    c : int;
    w : int;
    h : int;
  }

type piece = TL | BR | BL | TR | V | H | T | Tu | C

val pieces : int array

val int_of_piece : piece -> int

module type Api = sig

  type ctx
  type style 

  val get_bounds : ctx -> rect

  val get_style : Style.t -> style

  val clear : ctx -> unit

  val fill : 
    ctx:ctx -> style:style -> bounds:rect -> 
    char -> unit

  val draw_piece : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> piece -> unit

  val draw_char : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> char -> unit

  val draw_string : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> string -> unit

  val draw_box : 
    ctx:ctx -> style:style -> bounds:rect ->
    string -> unit

end

module type Brick = sig

  type ctx
  type style 

  val get_bounds : ctx -> rect

  val get_style : Style.t -> style

  val draw_char : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> char -> unit

  val draw_piece : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> piece -> unit

end

module Build(B : Brick) : Api
  with type ctx = B.ctx

module In_memory : sig

  type point = int * Style.t

  module Api : Api 
    with type ctx = point array array

  val init : rows:int -> cols:int -> Api.ctx

end

