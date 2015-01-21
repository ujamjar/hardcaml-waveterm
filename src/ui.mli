module Make (B : HardCaml.Comb.S)
            (W : HardCamlWaveTerm.Wave.W with type elt = B.t) : sig

  module G : HardCamlWaveTerm.Gfx.Api

  val draw : G.ctx -> W.waves -> unit

  val loop : ?timeout:float -> LTerm_ui.t -> W.waves -> unit Lwt.t

  val init : W.waves -> LTerm_ui.t Lwt.t

  (** Run wave viewer UI *)
  val run : ?timeout:float -> W.waves -> unit Lwt.t

  (** Run testbench and wave viewer UI.  Returns None if UI is quit before
      the testbench completes.  The UI will (optionally) update every [timeout]
      seconds. *)
  val run_testbench : ?timeout:float -> W.waves -> 'a Lwt.t -> 'a option Lwt.t

end
