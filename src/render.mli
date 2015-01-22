module Styles : sig
  type t =
    {
      style : Gfx.Style.t;
      border : Gfx.Style.t option;
      signals : Gfx.Style.t;
      values : Gfx.Style.t;
      waves : Gfx.Style.t;
      status : Gfx.Style.t;
    }

  val default : Gfx.Style.t -> t
  val black_on_white : t
  val white_on_black : t
  val colour : t -> t
  val colour_on_white : t
  val colour_on_black : t
end

module Bounds : sig
  type t = 
    {
      signals : Gfx.rect;
      values : Gfx.rect;
      waves : Gfx.rect;
      status : Gfx.rect;
    }
  val fit_to_window : 
    ?signals:bool -> ?values:bool -> ?waves:bool -> ?status:bool -> 
    Gfx.rect -> t
end

(** Functions for drawing waves, signal names and values *)
module Make (G : Gfx.Api) (W : Wave.W) : sig

  (** get width code and actual width in chars *)
  val get_wave_width : int * W.wave -> int * int

  (** get height code and actual height in chars *)
  val get_wave_height : int * W.wave -> int * int

  (** max width of name window *)
  val get_max_signal_width : W.waves -> int

  (** max width of values window *)
  val get_max_value_width : W.waves -> int

  (** max no of wave cycles *)
  val get_max_cycles : W.waves -> int

  (** max width of wave window *)
  val get_max_wave_width : W.waves -> int

  (** max height of wave window *)
  val get_max_wave_height : W.waves -> int

  (** draws one clock cycle *)
  val draw_clock_cycle : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> c:int -> unit

  (** draws [cnt] clock cycles *)
  val draw_clock_cycles : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> waw:int -> h:int -> cnt:int -> unit

  (** draw binary waveform data *)
  val draw_binary_data : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> data:W.t -> off:int -> unit

  (** draw arbitrary waveform data *)
  val draw_data : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> to_str:(W.elt -> string) -> 
    w:int -> h:int -> data:W.t -> off:int -> unit

  (** draw cursor *)
  val draw_cursor : ctx:G.ctx -> bounds:Gfx.rect -> state:W.waves -> int -> unit

  (** draw waveforms *)
  val draw_wave : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> W.waves -> unit
  
  (** draw signal names *)
  val draw_signals : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> W.waves -> unit

  (** draw signal values *)
  val draw_values : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> W.waves -> unit

  val draw_status :
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> W.waves -> unit

  (** draw standard user inferface (names, values, waveforms left to right *)
  val draw_ui :
    ?style:Styles.t -> ?bounds:Bounds.t -> 
    ctx:G.ctx -> W.waves -> unit 

end

module Static(W : Wave.W) : sig

  module R : module type of Make(Gfx.In_memory.Api)(W)

  val draw : 
    ?signals:bool -> ?values:bool -> ?waves:bool -> ?style:Styles.t ->
    ?rows:int -> ?cols:int -> W.waves -> 
    Gfx.In_memory.Api.ctx

  val draw_full : 
    ?style:Styles.t -> W.waves -> 
    Gfx.In_memory.Api.ctx * 
    Gfx.In_memory.Api.ctx * 
    Gfx.In_memory.Api.ctx

end

