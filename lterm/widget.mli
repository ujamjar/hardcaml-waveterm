module Make
  (B : HardCaml.Comb.S) 
  (W : HardCamlWaveTerm.Wave.W with type elt = B.t) 
: sig

  module G : module type of Gfx_lterm.Api
  module R : module type of HardCamlWaveTerm.Render.Make(G)(W)

  class waves : W.waves -> object
    inherit LTerm_widget.t
    method hscroll : LTerm_widget.scrollable
    method vscroll : LTerm_widget.scrollable
    method document_size : LTerm_geom.size
    method page_size : LTerm_geom.size
    method wheel_event : LTerm_widget.scrollable -> LTerm_event.t -> bool
    method scale_event : LTerm_event.t -> bool
  end

  class signals : int -> W.waves -> waves -> object
    inherit LTerm_widget.t
    method hscroll : LTerm_widget.scrollable
  end

  class values : int -> W.waves -> waves -> object
    inherit LTerm_widget.t
    method hscroll : LTerm_widget.scrollable
  end

  class status : W.waves -> LTerm_widget.t

  class waveform : W.waves -> object
    inherit LTerm_widget.hbox
    method waves : waves
    method values : values
    method signals : signals
  end

end

