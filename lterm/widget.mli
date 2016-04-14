module Make
  (B : HardCaml.Comb.S) 
  (W : HardCamlWaveTerm.Wave.W with type elt = B.t) 
: sig

  module G : module type of Gfx_lterm.Api
  module R : module type of HardCamlWaveTerm.Render.Make(G)(W)

  class waves : object
    inherit LTerm_widget.t
    method set_waves : W.waves -> unit
    method hscroll : LTerm_widget.scrollable
    method vscroll : LTerm_widget.scrollable
    method document_size : LTerm_geom.size
    method update_wave_cycles : unit
    method page_size : LTerm_geom.size
    method wheel_event : LTerm_widget.scrollable -> LTerm_event.t -> bool
    method scale_event : LTerm_event.t -> bool
    method key_scroll_event : LTerm_widget.scrollable -> LTerm_event.t -> bool
  end

  class signals : int -> waves -> object
    inherit LTerm_widget.t
    method set_waves : W.waves -> unit
    method hscroll : LTerm_widget.scrollable
  end

  class values : int -> waves -> object
    inherit LTerm_widget.t
    method set_waves : W.waves -> unit
    method hscroll : LTerm_widget.scrollable
  end

  class status : object
    inherit LTerm_widget.t
    method set_waves : W.waves -> unit
  end

  class waveform : ?signals_width:int -> ?values_width:int -> unit -> object
    inherit LTerm_widget.hbox
    method waves : waves
    method values : values
    method signals : signals
    method set_waves : ?keep_cfg:bool -> W.waves -> unit
    method update_wave_cycles : unit
  end

  val run : ?exit:(unit Lwt.t * unit Lwt.u) -> #LTerm_widget.t -> unit Lwt.t

  val run_testbench : ?exit:(unit Lwt.t * unit Lwt.u) -> #LTerm_widget.t -> 
    'a Lwt.t -> 'a option Lwt.t

end


