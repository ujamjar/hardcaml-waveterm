module Api_old = struct (* delete me! *)

  open CamomileLibrary
  open Gfx

  (* XXX remove user_ctx, add draw_uchar *)

  type user_ctx = LTerm_draw.matrix * LTerm_ui.t
  type ctx = LTerm_draw.context
  type style = LTerm_style.t

  let get_context (matrix,ui) = 
    let size = LTerm_ui.size ui in
    let ctx = LTerm_draw.context matrix size in
    ctx, LTerm_geom.({ r = 0; c = 0; w = size.cols; h = size.rows })

  let get_colour c = 
    let open LTerm_style in
    let open Gfx.Style in
    match c with
    | Black -> black | Red -> red | Green -> green | Yellow -> yellow 
    | Blue -> blue | Magenta -> magenta | Cyan -> cyan | White -> white
    | LBlack -> lblack | LRed -> lred | LGreen -> lgreen | LYellow -> lyellow 
    | LBlue -> lblue | LMagenta -> lmagenta | LCyan -> lcyan | LWhite -> lwhite

  let get_style style = 
    { LTerm_style.none with 
      LTerm_style.foreground = Some (get_colour style.Gfx.Style.fg);
      LTerm_style.background = Some (get_colour style.Gfx.Style.bg);
      LTerm_style.bold = Some style.Gfx.Style.bold; }

  let clear ctx = LTerm_draw.clear ctx

  let pieces = 
    let open LTerm_draw in
    let back = Blank in
    let front = Light in
    let blank = { top=back; bottom=back; left=back; right=back } in
    [|
      { blank with left=front; top=front };
      { blank with right=front; bottom=front };
      { blank with left=front; bottom=front };
      { blank with right=front; top=front };
      { blank with top=front; bottom=front };
      { blank with left=front; right=front };
      { blank with left=front; right=front; bottom=front };
      { blank with left=front; right=front; top=front };
      { left=front; right=front; top=front; bottom=front };
    |]

  let get_piece = function
    | TL -> pieces.(0)
    | BR -> pieces.(1)
    | BL -> pieces.(2)
    | TR -> pieces.(3)
    | V  -> pieces.(4)
    | H  -> pieces.(5)
    | T  -> pieces.(6)
    | Tu -> pieces.(7)
    | C  -> pieces.(8)

  let fill ~ctx ~style ~bounds ch = 
    let ch = UChar.of_char ' ' in
    for r=bounds.r to bounds.r + bounds.h - 1 do
      for c=bounds.c to bounds.c + bounds.w - 1 do
        LTerm_draw.draw_char 
          ctx ~style (bounds.r + r) (bounds.c + c) ch
      done;
    done

  let draw_piece ~ctx ~style ~bounds ~r ~c piece = 
    if r < bounds.h && c < bounds.w then begin
      let piece = get_piece piece in
      LTerm_draw.draw_piece ctx ~style (bounds.r + r) (bounds.c + c) piece
    end

  let draw_char ~ctx ~style ~bounds ~r ~c ch =
    if r < bounds.h && c < bounds.w then begin
      LTerm_draw.draw_char 
        ctx ~style (bounds.r + r) (bounds.c + c) (UChar.of_char ch) 
    end

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

module Brick = struct

  open CamomileLibrary
  open Gfx

  (* XXX remove user_ctx, add draw_uchar *)

  type user_ctx = LTerm_draw.matrix * LTerm_ui.t
  type ctx = LTerm_draw.context
  type style = LTerm_style.t

  let get_context (matrix,ui) = 
    let size = LTerm_ui.size ui in
    let ctx = LTerm_draw.context matrix size in
    ctx, LTerm_geom.({ r = 0; c = 0; w = size.cols; h = size.rows })

  let get_bounds ctx =
    let size = LTerm_draw.size ctx in
    Gfx.{ r=0; c=0; h=size.LTerm_geom.rows; w=size.LTerm_geom.cols }

  let get_colour c = 
    let open LTerm_style in
    let open Gfx.Style in
    match c with
    | Black -> black | Red -> red | Green -> green | Yellow -> yellow 
    | Blue -> blue | Magenta -> magenta | Cyan -> cyan | White -> white
    | LBlack -> lblack | LRed -> lred | LGreen -> lgreen | LYellow -> lyellow 
    | LBlue -> lblue | LMagenta -> lmagenta | LCyan -> lcyan | LWhite -> lwhite

  let get_style style = 
    { LTerm_style.none with 
      LTerm_style.foreground = Some (get_colour style.Gfx.Style.fg);
      LTerm_style.background = Some (get_colour style.Gfx.Style.bg);
      LTerm_style.bold = Some style.Gfx.Style.bold; }

  let clear ctx = LTerm_draw.clear ctx

  let pieces = 
    let open LTerm_draw in
    let back = Blank in
    let front = Light in
    let blank = { top=back; bottom=back; left=back; right=back } in
    [|
      { blank with left=front; top=front };
      { blank with right=front; bottom=front };
      { blank with left=front; bottom=front };
      { blank with right=front; top=front };
      { blank with top=front; bottom=front };
      { blank with left=front; right=front };
      { blank with left=front; right=front; bottom=front };
      { blank with left=front; right=front; top=front };
      { left=front; right=front; top=front; bottom=front };
    |]

  let get_piece = function
    | TL -> pieces.(0)
    | BR -> pieces.(1)
    | BL -> pieces.(2)
    | TR -> pieces.(3)
    | V  -> pieces.(4)
    | H  -> pieces.(5)
    | T  -> pieces.(6)
    | Tu -> pieces.(7)
    | C  -> pieces.(8)

  let rows ctx = (LTerm_draw.size ctx).LTerm_geom.rows
  let cols ctx = (LTerm_draw.size ctx).LTerm_geom.cols

  let draw_piece ~ctx ~style ~bounds ~r ~c piece = 
    if r < bounds.h && c < bounds.w then begin
      let piece = get_piece piece in
      LTerm_draw.draw_piece ctx ~style (bounds.r + r) (bounds.c + c) piece
    end

  let draw_char ~ctx ~style ~bounds ~r ~c ch =
    if r < bounds.h && c < bounds.w then begin
      LTerm_draw.draw_char 
        ctx ~style (bounds.r + r) (bounds.c + c) (UChar.of_char ch) 
    end

end

module Api = Gfx.Build(Brick)

