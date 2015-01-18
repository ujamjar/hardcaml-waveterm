(** Lambda-term based gfx API *)
module Api : Gfx.Api
  with type ctx = LTerm_draw.context
