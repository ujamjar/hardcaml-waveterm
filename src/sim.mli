open HardCaml

module Make(B : Comb.S) : sig

  val wrap : B.t Cyclesim.Api.cyclesim -> 
    B.t Cyclesim.Api.cyclesim * (unit -> unit)

end
