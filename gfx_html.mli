type styler = 
  {
    start : (string -> unit) -> unit;
    set : (string -> unit) -> Gfx.Style.t -> unit;
    eol : (string -> unit) -> unit;
    finish : (string -> unit) -> unit;
  }

val html_styler : styler
val term_styler : styler

val write_html_escape : ?styler:styler -> (string -> unit) -> Gfx.In_memory.Api.ctx -> unit

val write_utf8 : ?styler:styler -> (string -> unit) -> Gfx.In_memory.Api.ctx -> unit

