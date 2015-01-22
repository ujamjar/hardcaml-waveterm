(** Styling information *)
module Style : sig
  (** colour *)
  type colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  (** foreground/backgound colours and weight (bold) *)
  type t = 
    {
      bold : bool;
      fg : colour;
      bg : colour;
    }
  (** white on black, normal weight *)
  val default : t
end

(** A rectangle (no really) *)
type rect = 
  {
    r : int; (** rows *)
    c : int; (** cols *)
    w : int; (** width *)
    h : int; (** height *)
  }

type piece = 
  (** corners, vert/horz bar, T shapes, cross *)
  TL | BR | BL | TR | V | H | T | Tu | C |
  (* full/half blocks *)
  F | TH | BH | LH | RH | 
  (* quarter blocks *)
  QTL | QBR | QBL | QTR

(** unicode value of piece *)
val unicode_of_piece : piece -> int

(** Main graphics drawing API.
    
    Most functions take a [bounds] parameter which is a rectangle
    to which drawing is clipped and also serves as an origin for
    any coordinates. *)
module type Api = sig

  (** drawing context *)
  type ctx

  (** underlying style type *)
  type style 

  (** get context size *)
  val get_bounds : ctx -> rect

  (** convert our style info to underlying style *)
  val get_style : Style.t -> style

  (** clear display *)
  val clear : ctx -> unit

  (** fill [bounds] with [char] given [style] *) 
  val fill : 
    ctx:ctx -> style:style -> bounds:rect -> 
    char -> unit

  (** draw int (representing unicode value) *)
  val draw_int : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> int -> unit

  (** draw piece *)
  val draw_piece : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> piece -> unit
  
  (** draw char *)
  val draw_char : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> char -> unit

  (** draw string (nothing fancy - horizontal, no breaks) *)
  val draw_string : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> string -> unit

  (** draw box outline with label *)
  val draw_box : 
    ctx:ctx -> style:style -> bounds:rect ->
    string -> unit
  
  (** get value and style at point *)
  val get : ctx:ctx -> bounds:rect -> r:int -> c:int -> int * Style.t

  (** invert fg and bg at point *)
  val inv : ctx:ctx -> bounds:rect -> r:int -> c:int -> unit

  (** set bold on point *)
  val bold : ctx:ctx -> bounds:rect -> r:int -> c:int -> unit

end

(** The basic functions needed to build the full API *)
module type Brick = sig

  type ctx
  type style 

  val get_bounds : ctx -> rect

  val get_style : Style.t -> style

  val draw_int : 
    ctx:ctx -> style:style -> bounds:rect ->
    r:int -> c:int -> int -> unit

  val get : ctx:ctx -> bounds:rect -> r:int -> c:int -> int * Style.t

end

(** Construct the API from a Brick implementation *)
module Build(B : Brick) : Api
  with type ctx = B.ctx

(** In memory based API with no external requirements *)
module In_memory : sig

  type point = int * Style.t

  module Api : Api 
    with type ctx = point array array

  val init : rows:int -> cols:int -> Api.ctx

end

