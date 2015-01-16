type styler = 
  {
    start : (string -> unit) -> unit;
    set : (string -> unit) -> Gfx.Style.t -> unit;
    eol : (string -> unit) -> unit;
    finish : (string -> unit) -> unit;
  }

val no_styler : styler
val html_styler : styler
val term_styler : styler

val html_escape : ?styler:styler -> (string -> unit) -> Gfx.In_memory.Api.ctx -> unit

val utf8 : ?styler:styler -> (string -> unit) -> Gfx.In_memory.Api.ctx -> unit

