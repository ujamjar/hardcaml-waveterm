module Make (B : HardCaml.Comb.S)
            (W : HardCamlWaveTerm.Wave.W with type elt = B.t) : sig

  module G : HardCamlWaveTerm.Gfx.Api

  type state

  val loop : ?timeout:float -> (LTerm_ui.t * state) -> unit Lwt.t

  val init : 
    ?style:HardCamlWaveTerm.Render.Styles.t ->
    W.waves -> (LTerm_ui.t * state * LTerm.t) Lwt.t

  (** Run wave viewer UI *)
  val run : 
    ?style:HardCamlWaveTerm.Render.Styles.t ->
    ?timeout:float -> W.waves -> unit Lwt.t

  (** Run testbench and wave viewer UI.  Returns None if UI is quit before
      the testbench completes.  The UI will (optionally) update every [timeout]
      seconds. *)
  val run_testbench : 
    ?style:HardCamlWaveTerm.Render.Styles.t ->
    ?timeout:float -> W.waves -> 'a Lwt.t -> 'a option Lwt.t

end

