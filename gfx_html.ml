type point = CamomileLibrary.UChar.t * Gfx.Style.t

module Brick = struct

  open CamomileLibrary
  open Gfx

  type user_ctx = point array array 
  type ctx = user_ctx
  type style = Gfx.Style.t

  let rows ctx = Array.length ctx
  let cols ctx = try Array.length ctx.(0) with _ -> 0

  let get_context ctx = ctx, { r=0; c=0; h=rows ctx; w=cols ctx } (* XXX remove rect! *)
 
  let get_bounds ctx = { r=0; c=0; h=rows ctx; w=cols ctx }

  let get_style s = s

  let draw_char ~ctx ~style ~bounds ~r ~c ch = 
    ctx.(bounds.r + r).(bounds.c + c) <- UChar.of_char ch, style

  let draw_piece ~ctx ~style ~bounds ~r ~c piece = 
    draw_char ~ctx ~style ~bounds ~r ~c ' ' (* find codes! *)

end

module Api = Gfx.Build(Brick)

