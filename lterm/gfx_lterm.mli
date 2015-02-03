(** Lambda-term based gfx API *)
module Api : HardCamlWaveTerm.Gfx.Api
  with type ctx = LTerm_draw.context
