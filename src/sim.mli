open HardCaml

module Make(B : Comb.S) : sig

  module D : module type of Wave.Bits(B)
  module W : module type of Wave.Make(D)
  module R : module type of Render.Static(W)

  val wrap : B.t Cyclesim.Api.cyclesim -> 
    B.t Cyclesim.Api.cyclesim * R.R.t

end
