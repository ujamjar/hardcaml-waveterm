open HardCaml

module Make(B : Comb.S)(W : Wave.W with type elt = B.t) : sig

  val wrap :
    ?cfg:(string * W.to_str) list ->
    B.t Cyclesim.Api.cyclesim -> 
    B.t Cyclesim.Api.cyclesim * W.wave array

end
