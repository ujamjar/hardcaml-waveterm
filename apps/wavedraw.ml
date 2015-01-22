(* HTML or UFT-8 file generation with various options *)
open HardCamlWaveTerm

let help = 
"Wave draw
=========

Render HardCaml digital waveform files into UTF-8 text or
HTML Files.

$ " ^ Sys.argv.(0) ^ " {[options] [-o file-out] file-in}*

By default output is written as UTF-8 without any styling.  
This is suitable for loading into editors.   Enable 
styling with '-styler term' (view with 'less -r file').

Static or scrollable HTML output can be selected with 
the '-html' option.  Styling is enabled with the 'css' 
or 'class' styler options.

Mutliple outputs can be generated.  This happens
whenever a file-in argument is read.  Therefore,
the output file, if any, and arguments must be specified 
before the corresponding input file.

The output file and mode are reset to stdout and UTF-8
between each invocation. ie

$ " ^ Sys.argv.(0) ^ " a.wave -html static -o b.html b.wave c.wave

will (1) display a.wave on stdout (2) write b.html from b.wave 
(3) display c.wave on stdout.
"

type mode = Utf8 | Html | Html_scroll

type styler = No_style | Css | Css_class | Term

(* command line *)
let rows, cols = ref 0, ref 80
let width, height = ref 3, ref 1
let start_cycle, start_signal = ref 0, ref 0
let cursor = ref (-1)
let styler = ref No_style
let scheme = ref Render.Styles.colour_on_black
let mode = ref Utf8
let out_file = ref ""

let reset_state () = mode := Utf8; out_file := ""

module B = HardCaml.Bits.Comb.IntbitsList
module W = Wave.Make(Wave.Bits(B))
module G = Gfx.In_memory.Api
module R = Render.Static(W)
open Gfx
open G

(* static user interface *)
let draw_static waves = 
  R.draw ~style:!scheme 
    ?rows:(if !rows=0 then None else Some(!rows))
    ~cols:!cols waves

(* draw everything at full size *)
let draw_scroll waves = 
  let sctx, _, wctx = R.draw_full ~style:!scheme waves in
  sctx, wctx

let get_waves name = 
  let f = open_in name in
  let w = W.read f in
  close_in f;
  w

let gen name = begin
  (* set up output file *)
  let os, close = 
    if !out_file = "" then 
      print_string, (fun () -> ())
    else 
      let f = open_out !out_file in
      output_string f, (fun () -> close_out f)
  in
  let waves = W.({ (get_waves name) with
    cfg = { default with (* XXX live with warning for now ... *)
      wave_width = !width;
      wave_height = !height;
      start_cycle = !start_cycle;
      start_signal = !start_signal;
      wave_cursor = !cursor;
    }
  }) in
  let style_fn = 
    match !styler with
    | No_style -> Write.no_styler
    | Term -> Write.term_styler
    | Css -> Write.html_styler
    | Css_class -> Write.css_class_styler
  in

  let html_header () = 
    if !styler = Css_class then begin
      (* write embedded css classes *)
      os 
        ("<html><head><meta charset=\"UTF-8\"><style>" ^ 
        Write.css_classes ^ 
        "</style></head>\n")
    end else begin
      os "<html><head><meta charset=\"UTF-8\"></head>\n";
    end;
  in

  begin
    match !mode with
    | Utf8 -> begin
      (* write utf-8 *)
      let ctx = draw_static waves in
      Write.utf8 ~styler:style_fn os ctx
    end
    | Html -> begin
      (* write html file *)
      let ctx = draw_static waves in
      html_header ();
      os "<body><pre>\n";
      Write.html_escape ~styler:style_fn os ctx;
      os "</pre></body>"
    end
    | Html_scroll -> begin
      (* write html file with signals and waves as floating divs with scroll bars.
      * bit of a hack for the width %'s.  Not really sure how its supposed to
      * work, but it seems to be OK. *)
      let sctx, wctx = draw_scroll waves in
      let div_style = 
        "display:inline-block; overflow-x:auto; float:left" 
      in

      html_header ();
      os "<body>\n";
      os "<div style=\"margin-left:5%; width:100%\">";
      os ("<div style=\"max-width:20%; " ^ div_style ^ "\"><pre>");
      Write.html_escape ~styler:style_fn os sctx;
      os "</pre></div>\n";
      os ("<div style=\"max-width:80%; " ^ div_style ^ "\"><pre>");
      Write.html_escape ~styler:style_fn os wctx;
      os "</pre></div>\n";
      os "<div style=\"clear:both\"></div>";
      os "</body>";
    end
  end;
  (* reset out file and mode, carry over various styling infos *)
  close (); reset_state ()
end

let () = 
  if Array.length Sys.argv = 1 then print_string help 
  else Arg.parse
  [
    "-rows", Arg.Set_int rows, "number of rows";
    "-cols", Arg.Set_int cols, "number of cols";
    "-width", Arg.Set_int width, "wave cycle width";
    "-height", Arg.Set_int height, "wave cycle height";
    "-cycle", Arg.Set_int start_cycle, "wave start cycle";
    "-signal", Arg.Set_int start_signal, "wave start signal";
    "-cursor", Arg.Set_int cursor, "cursor";
    "-styler", Arg.Symbol(["none"; "term"; "css"; "class"],
      (function
        | "term" -> styler := Term 
        | "css" -> styler := Css
        | "class" -> styler := Css_class
        | "none" -> styler := No_style
        | _ -> ())), " select style generator";
    "-style", Arg.Symbol(["white"; "black"; "light"; "dark"],
      (function
        | "white" -> scheme := Render.Styles.black_on_white
        | "black" -> scheme := Render.Styles.white_on_black
        | "light" -> scheme := Render.Styles.colour_on_white
        | "dark" -> scheme := Render.Styles.colour_on_black
        | _ -> ())), " select colour scheme";
    "-html", Arg.Symbol(["static"; "scroll"], 
      (function
        | "static" -> mode := Html
        | "scroll" -> mode := Html_scroll
        | _ -> ())), " HTML generation";
    "-o", Arg.Set_string out_file, "output file (default stdout)";
  ]
  gen help

