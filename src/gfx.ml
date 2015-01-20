module Style = struct
  type colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  type t = 
    {
      bold : bool;
      fg : colour;
      bg : colour;
    }
  let default = 
    {
      bold = false;
      fg = White;
      bg = Black;
    }
end

type rect = 
  {
    r : int;
    c : int;
    w : int;
    h : int;
  }

type piece = 
  (* lines *)
  TL | BR | BL | TR | V | H | T | Tu | C |
  (* full/half blocks *)
  F | TH | BH | LH | RH | 
  (* quarter blocks *)
  QTL | QBR | QBL | QTR

let bars = false (* kinda interesting *)

let unicode_of_piece = function
  | TL  -> 0x2518
  | BR  -> 0x250c
  | BL  -> 0x2510
  | TR  -> 0x2514
  | V   -> 0x2502
  | H   -> 0x2500
  | T   -> 0x252c
  | Tu  -> 0x2534
  | C   -> 0x253c
  | F   -> if bars then 0x2551 else 0x2588
  | TH  -> if bars then 0x2568 else 0x2580
  | BH  -> if bars then 0x2565 else 0x2584
  | LH  -> 0x258c
  | RH  -> 0x2590
  | QTL -> 0x2598
  | QBR -> 0x2597
  | QBL -> 0x2596
  | QTR -> 0x259d

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

module Build(B : Brick) = struct

  include B

  let fill ~ctx ~style ~bounds ch = 
    for r=bounds.r to bounds.r + bounds.h - 1 do
      for c=bounds.c to bounds.c + bounds.w - 1 do
        draw_char ~ctx ~style ~bounds ~r ~c ch 
      done;
    done

  let clear ctx = 
    let bounds = get_bounds ctx in
    let style = get_style Style.default in
    for r=0 to bounds.h - 1 do
      for c=0 to bounds.w - 1 do
        draw_char ~ctx ~style ~bounds ~r ~c ' '
      done
    done

  let draw_string ~ctx ~style ~bounds ~r ~c str = 
    for i=0 to String.length str - 1 do
      draw_char ~ctx ~style ~bounds ~r ~c:(c+i) str.[i]
    done

  let draw_box ~ctx ~style ~bounds label = 
    let w, h = bounds.w, bounds.h in
    assert (w>=2 && h>=2); (* min box size including borders *)
    draw_piece ~ctx ~style ~bounds ~r:0 ~c:0 BR;
    draw_piece ~ctx ~style ~bounds ~r:(h-1) ~c:0 TR;
    draw_piece ~ctx ~style ~bounds ~r:0 ~c:(w-1) BL;
    draw_piece ~ctx ~style ~bounds ~r:(h-1) ~c:(w-1) TL;
    for c=1 to (w-2) do draw_piece ~ctx ~style ~bounds ~r:0 ~c H done;
    for c=1 to (w-2) do draw_piece ~ctx ~style ~bounds ~r:(h-1) ~c H done;
    for r=1 to (h-2) do draw_piece ~ctx ~style ~bounds ~r ~c:0 V done;
    for r=1 to (h-2) do draw_piece ~ctx ~style ~bounds ~r ~c:(w-1) V done;
    draw_string ~ctx ~style ~bounds:{bounds with w=w-1} ~r:0 ~c:1 label

end

module In_memory = struct

  type point = int * Style.t

  module Brick = struct

    type ctx = point array array 
    type style = Style.t

    let rows ctx = Array.length ctx
    let cols ctx = try Array.length ctx.(0) with _ -> 0

    let get_bounds ctx = { r=0; c=0; h=rows ctx; w=cols ctx }

    let get_style s = s

    let draw_char ~ctx ~style ~bounds ~r ~c ch = 
      if r >=0 && r < bounds.h && c >= 0 && c < bounds.w then begin
        ctx.(bounds.r + r).(bounds.c + c) <- Char.code ch, style
      end

    let draw_piece ~ctx ~style ~bounds ~r ~c piece = 
      if r >=0 && r < bounds.h && c >= 0 && c < bounds.w then begin
        ctx.(bounds.r + r).(bounds.c + c) <- unicode_of_piece piece, style
      end

  end

  module Api = Build(Brick)

  let init ~rows ~cols = 
    let ch = Char.code ' ' in
    Array.init rows (fun r -> Array.init cols (fun c -> ch, Style.default))

end

