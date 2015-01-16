module Brick = struct

  open CamomileLibrary
  open Gfx

  type ctx = LTerm_draw.context
  type style = LTerm_style.t

  let get_bounds ctx =
    let size = LTerm_draw.size ctx in
    Gfx.{ r=0; c=0; h=size.LTerm_geom.rows; w=size.LTerm_geom.cols }

  let get_colour c = 
    let open LTerm_style in
    let open Gfx.Style in
    match c with
    | Black -> black | Red -> red | Green -> green | Yellow -> yellow 
    | Blue -> blue | Magenta -> magenta | Cyan -> cyan | White -> white

  let get_style style = 
    { LTerm_style.none with 
      LTerm_style.foreground = Some (get_colour style.Gfx.Style.fg);
      LTerm_style.background = Some (get_colour style.Gfx.Style.bg);
      LTerm_style.bold = Some style.Gfx.Style.bold; }

  let clear ctx = LTerm_draw.clear ctx

  let draw_piece ~ctx ~style ~bounds ~r ~c piece = 
    if r >=0 && r < bounds.h && c >= 0 && c < bounds.w then begin
      LTerm_draw.draw_char ctx ~style (bounds.r + r) (bounds.c + c) 
        (UChar.of_int Gfx.pieces.(Gfx.int_of_piece piece))
    end

  let draw_char ~ctx ~style ~bounds ~r ~c ch =
    if r >=0 && r < bounds.h && c >= 0 && c < bounds.w then begin
      LTerm_draw.draw_char 
        ctx ~style (bounds.r + r) (bounds.c + c) (UChar.of_char ch) 
    end

end

module Api = Gfx.Build(Brick)

