open HardCaml

module Make(B : Comb.S) : sig

  module D : Wave.D
  module W : Wave.W
  module R : module type of Render.Static(W)

  val wrap : B.t Cyclesim.Api.cyclesim -> 
    B.t Cyclesim.Api.cyclesim * R.R.t

end
