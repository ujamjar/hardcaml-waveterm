module Style : sig
  type colour = 
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    | LBlack | LRed | LGreen | LYellow | LBlue | LMagenta | LCyan | LWhite
  type t = 
    {
      bold : bool;
      fg : colour;
      bg : colour;
    }
end

type rect = 
  {
    r : int;
    c : int;
    w : int;
    h : int;
  }

type piece = TL | BR | BL | TR | V | H | T | Tu | C

module type Api = sig

  type user_ctx
  type ctx
  type style 

  val get_context : user_ctx -> ctx * rect

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

  type user_ctx
  type ctx
  type style 

  val get_context : user_ctx -> ctx * rect

  val get_bounds : ctx -> rect

  val get_style : Style.t -> style

  val draw_char : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> char -> unit

  val draw_piece : (* can probably get rid of this! need unicodes! *)
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> piece -> unit

end

module Build(B : Brick) : Api
  with type user_ctx = B.user_ctx

