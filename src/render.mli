module Make (G : Gfx.Api) (W : Wave.S) : sig

  type wt = W.t
  type t = 
    {
      mutable wave_width : int;
      mutable wave_height : int;
      mutable wave_cycle : int;
      waves : wt Wave.t array;
    }

  val get_wave_width : int * wt Wave.wave -> int * int

  val get_wave_height : int * wt Wave.wave -> int * int

  val draw_clock_cycle : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> c:int -> unit

  val draw_clock_cycles : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> waw:int -> h:int -> cnt:int -> unit

  val draw_binary_data : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> data:W.t -> off:int -> cnt:int -> unit

  val draw_data : ctx:G.ctx -> style:G.style -> bounds:Gfx.rect -> 
    w:int -> h:int -> data:W.t -> off:int -> cnt:int -> unit

  val draw_wave : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> state:t -> unit -> unit
  
  val draw_signals : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> state:t -> unit -> unit

  val draw_values : 
    ?style:Gfx.Style.t -> ?border:Gfx.Style.t ->
    ctx:G.ctx -> bounds:Gfx.rect -> state:t -> unit -> unit

end
  with type wt = W.t
