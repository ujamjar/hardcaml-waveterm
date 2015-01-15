type point = CamomileLibrary.UChar.t * Gfx.Style.t

module Api : Gfx.Api 
  with type user_ctx = point array array 
