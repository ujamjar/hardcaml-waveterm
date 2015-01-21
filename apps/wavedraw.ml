(* HTML or UFT-8 file generation with various options *)
open HardCamlWaveTerm

type mode = Utf8 | Html | Html_scroll

type styler = No_style | Css | Css_class | Term

(* command line *)
let rows, cols = ref 0, ref 80
let width, height, start = ref 3, ref 1, ref 0
let styler = ref No_style
let scheme = ref Render.Styles.colour_on_black
let mode = ref Utf8
let files = ref []

let () = Arg.parse
  [
    "-rows", Arg.Set_int rows, "number of rows";
    "-cols", Arg.Set_int cols, "number of cols";
    "-width", Arg.Set_int width, "wave cycle width";
    "-height", Arg.Set_int height, "wave cycle height";
    "-start", Arg.Set_int start, "wave start cycle";
    "-style", Arg.Symbol(["term"; "css"; "class"],
      (function
        | "term" -> styler := Term 
        | "css" -> styler := Css
        | "class" -> styler := Css_class
        | _ -> ())), " select style generator";
    "-scheme", Arg.Symbol(["white"; "black"; "colour"],
      (function
        | "white" -> scheme := Render.Styles.black_on_white
        | "black" -> scheme := Render.Styles.white_on_black
        | "colour" -> scheme := Render.Styles.colour_on_white
        | _ -> ())), " select colour scheme";
    "-html", Arg.Symbol(["static"; "scroll"], 
      (function
        | "static" -> mode := Html
        | "scroll" -> mode := Html_scroll
        | _ -> ())), " HTML generation";
  ]
  (fun s -> files := s :: !files) "wave drawings"

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

let gen name = 
  let waves = W.({ (get_waves name) with
    wave_width = !width;
    wave_height = !height;
    wave_cycle = !start;
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
      print_string 
        ("<html><head><meta charset=\"UTF-8\"><style>" ^ 
        Write.css_classes ^ 
        "</style></head>\n")
    end else begin
      print_string "<html><head><meta charset=\"UTF-8\"></head>\n";
    end;
  in

  match !mode with
  | Utf8 -> begin
    (* write utf-8 *)
    let ctx = draw_static waves in
    Write.utf8 ~styler:style_fn print_string ctx
  end
  | Html -> begin
    (* write html file *)
    let ctx = draw_static waves in
    html_header ();
    print_string "<body><pre>\n";
    Write.html_escape ~styler:style_fn print_string ctx;
    print_string "</pre></body>"
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
    print_string "<body>\n";
    print_string "<div style=\"margin-left:5%; width:100%\">";
    print_string ("<div style=\"max-width:20%; " ^ div_style ^ "\"><pre>");
    Write.html_escape ~styler:style_fn print_string sctx;
    print_string "</pre></div>\n";
    print_string ("<div style=\"max-width:80%; " ^ div_style ^ "\"><pre>");
    Write.html_escape ~styler:style_fn print_string wctx;
    print_string "</pre></div>\n";
    print_string "<div style=\"clear:both\"></div>";
    print_string "</body>"
  end

let () = List.iter gen (List.rev !files)

