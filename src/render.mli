(** Functions for drawing waves, signal names and values *)
module Make (G : Gfx.Api) (W : Wave.S) : sig

  type wt = W.t

  (** wave configuration and data *)
  type t = 
    {
      mutable wave_width : int; (** width of wave cycle *)
      mutable wave_height : int; (** height of wave cycle *)
      mutable wave_cycle : int; (** start cycle *)
      waves : wt Wave.t array; (** data *)
    }

  (** get width code and actual width in chars *)
  val get_wave_width : int * wt Wave.wave -> int * int

  (** get height code and actual height in chars *)
  val get_wave_height : int * wt Wave.wave -> int * int

  (** max width of name window *)
  val get_max_name_width : t -> int

  (** max no of wave cycles *)
  val get_max_cycles : t -> int

  (** max width of wave window *)
  val get_max_wave_width : t -> int

  (** max height of wave window *)
  val get_max_wave_height : t -> int

  (** draws one clock cycle *)
  val draw_clock_cycle : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> c:int -> unit

  (** draws [cnt] clock cycles *)
  val draw_clock_cycles : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> waw:int -> h:int -> cnt:int -> unit

  (** draw binary waveform data *)
  val draw_binary_data : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> data:W.t -> off:int -> cnt:int -> unit

  (** draw arbitrary waveform data *)
  val draw_data : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> data:W.t -> off:int -> cnt:int -> unit

  (** draw waveforms *)
  val draw_wave : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> state:t -> unit -> unit
  
  (** draw signal names *)
  val draw_signals : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> state:t -> unit -> unit

  (** draw signal values *)
  val draw_values : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> state:t -> unit -> unit

  (** draw standard user inferface (names, values, waveforms left to right *)
  val draw_ui :
    ?style:Gfx.Style.t ->
    ?sstyle:Gfx.Style.t -> ?vstyle:Gfx.Style.t -> ?wstyle:Gfx.Style.t ->
    ?border:Gfx.Style.t -> ctx:G.ctx ->
    sbounds:Gfx.rect -> vbounds:Gfx.rect -> wbounds:Gfx.rect ->
    state:t -> unit -> unit

end
  with type wt = W.t

