module Brick = struct

  open CamomileLibrary
  open Gfx

  type ctx = LTerm_draw.context
  type style = LTerm_style.t

  let get_bounds ctx =
    let size = LTerm_draw.size ctx in
    { r=0; c=0; h=size.LTerm_geom.rows; w=size.LTerm_geom.cols }

  let get_colour c = 
    let open LTerm_style in
    let open Style in
    match c with
    | Black -> black | Red -> red | Green -> green | Yellow -> yellow 
    | Blue -> blue | Magenta -> magenta | Cyan -> cyan | White -> white

  let of_colour c = 
    let open LTerm_style in
    let open Style in
    if c = black then Black
    else if c = red then Red
    else if c = green then Green
    else if c = yellow then Yellow
    else if c = blue then Blue
    else if c = magenta then Magenta
    else if c = cyan then Cyan
    else if c = white then White
    else Black (* ??? *)

  let get_style style = 
    { LTerm_style.none with 
      LTerm_style.foreground = Some (get_colour style.Style.fg);
      LTerm_style.background = Some (get_colour style.Style.bg);
      LTerm_style.bold = Some style.Style.bold; }

  let clear ctx = LTerm_draw.clear ctx

  let draw_int ~ctx ~style ~bounds ~r ~c i = 
    if r >=0 && r < bounds.h && c >= 0 && c < bounds.w then begin
      LTerm_draw.draw_char ctx ~style (bounds.r + r) (bounds.c + c) 
        (UChar.of_int i)
    end

  let get ~ctx ~bounds ~r ~c = 
    if r >=0 && r < bounds.h && c >= 0 && c < bounds.w then 
      let point = LTerm_draw.point ctx (bounds.r + r) (bounds.c + c) in
      UChar.int_of point.LTerm_draw.char, 
      Style.({ 
        bold = point.LTerm_draw.bold;
        fg = of_colour point.LTerm_draw.foreground;
        bg = of_colour point.LTerm_draw.background;
      })
    else
      raise (Invalid_argument "out of bounds")

end

module Api = Gfx.Build(Brick)

