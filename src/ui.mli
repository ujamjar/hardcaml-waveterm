module Make (B : HardCaml.Comb.S) : sig

  module D : HardCamlWaveTerm.Wave.D
    with type elt = B.t
  module W : HardCamlWaveTerm.Wave.W
    with type elt = B.t
     and type t = D.t
  module G : HardCamlWaveTerm.Gfx.Api
  module R : module type of HardCamlWaveTerm.Render.Make(G)(W)

  val draw : G.ctx -> R.t -> unit

  val loop : LTerm_ui.t -> R.t -> unit Lwt.t

  val init : R.t -> LTerm_ui.t Lwt.t

end
