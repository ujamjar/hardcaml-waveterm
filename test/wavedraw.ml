(* HTML or UFT-8 file generation with various options *)

(* colour scheme *)
type styling = 
  | Colour_on_black
  | Colour_on_white
  | Black_on_white
  | White_on_black

let black_on_white = Gfx.Style.{default with fg=Black; bg=White }
let white_on_black = Gfx.Style.{default with fg=White; bg=Black }
let colouring s = Gfx.Style.( 
    { s with fg=Blue }, 
    { s with fg=Yellow }, 
    { s with fg=Green }
  )

let get_styling s =
  let get s f = s, s, f s in
  let bnw s = s, s, s in
  match s with
  | Colour_on_black -> get white_on_black colouring
  | Colour_on_white -> get black_on_white colouring
  | Black_on_white -> get black_on_white bnw
  | White_on_black -> get white_on_black bnw

type mode = Utf8 | Html | Html_scroll

type styler = No_style | Css | Css_class | Term

(* command line *)
let rows, cols = ref 31, ref 80
let width, height = ref 3, ref 1
let styler = ref No_style
let scheme = ref Colour_on_black
let mode = ref Utf8

let () = Arg.parse
  [
    "-rows", Arg.Set_int rows, "number of rows";
    "-cols", Arg.Set_int cols, "number of cols";
    "-width", Arg.Set_int width, "wave cycle width";
    "-height", Arg.Set_int height, "wave cycle height ";
    "-style", Arg.Symbol(["term"; "css"; "class"],
      (function
        | "term" -> styler := Term 
        | "css" -> styler := Css
        | "class" -> styler := Css_class
        | _ -> ())), "select style generator";
    "-scheme", Arg.Symbol(["white"; "black"; "colour"],
      (function
        | "white" -> scheme := Black_on_white
        | "black" -> scheme := White_on_black
        | "colour" -> scheme := Colour_on_white
        | _ -> ())), "select colour scheme";
    "-html", Arg.Symbol(["static"; "scroll"], 
      (function
        | "static" -> mode := Html
        | "scroll" -> mode := Html_scroll
        | _ -> ())), "HTML generation";
  ]
  (fun _ -> ()) "wave drawings"


module G = Gfx.In_memory.Api
module R = Render.Make(G)(Wave.Int)
open Gfx
open G

type state = 
  {
    mutable signal_window_width : int;
    mutable value_window_width : int;
    mutable waveform_window_width : int;
    wave : R.t;
  }

let get_state cols wave_width wave_height = 
  Gfx.{
    signal_window_width = 10;
    value_window_width = 10;
    waveform_window_width = cols-20;
    wave = 
      R.{
        wave_width;
        wave_height;
        wave_cycle = 0;
        waves  = [|
          "clock", Wave.Clock;
          "a", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
          "b", Wave.Data(Array.init 50 (fun _ -> Random.int 1000-500));
          "c", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
          "gamma-omega", Wave.Data(Array.init 50 (fun _ -> Random.int 10));
          "beta", Wave.Data(Array.init 50 (fun _ -> Random.int 3));
          "delta", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
          "eye", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
          "enable", Wave.Data(Array.init 50 (fun _ -> Random.int 10-5));
          "clock2", Wave.Clock;
          "bubble", Wave.Data(Array.init 50 (fun _ -> Random.int 20-10));
          "fairy", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
        |];
      }
  }

let style, border, (sstyle, vstyle, wstyle) = get_styling !scheme 

(* static user interface *)
let draw_static () = 
  let state = get_state !cols !width !height in
  let ctx = Gfx.In_memory.init ~rows:!rows ~cols:!cols in

  let bounds = get_bounds ctx in

  let sbounds = { r=0; c=0; w=state.signal_window_width; h=bounds.h } in
  let vbounds = { r=0; c=sbounds.c+sbounds.w; w=state.value_window_width; h=bounds.h } in
  let wbounds = { r=0; c=vbounds.c+vbounds.w; w=state.waveform_window_width; h=bounds.h } in

  R.draw_ui
    ~style ~sstyle ~vstyle ~wstyle ~border
    ~ctx ~sbounds ~vbounds ~wbounds ~state:state.wave ();

  ctx

(* draw everything at full size *)
let draw_scroll () = 
  let state = get_state !cols !width !height in

  (* get windoe sizes *)
  let swidth = R.get_max_name_width state.wave in
  let wwidth = R.get_max_wave_width state.wave in
  let wheight = R.get_max_wave_height state.wave in

  (* render signal names and waves into different ctx's *)
  let style, border, (sstyle, vstyle, wstyle) = get_styling Colour_on_black in

  let sctx = Gfx.In_memory.init ~rows:(wheight+2) ~cols:(swidth+2) in
  let bounds = { r=0; c=0; w=swidth+2; h=wheight+2 } in
  R.draw_signals ~style:sstyle ~border ~ctx:sctx ~bounds ~state:state.wave ();

  let wctx = Gfx.In_memory.init ~rows:(wheight+2) ~cols:(wwidth+2) in
  let bounds = { r=0; c=0; w=wwidth+2; h=wheight+2 } in
  R.draw_wave ~style:wstyle ~border ~ctx:wctx ~bounds ~state:state.wave ();
  sctx, wctx

let () = 
  let style_fn = 
    match !styler with
    | No_style -> Write.no_styler
    | Term -> Write.term_styler
    | Css -> Write.html_styler
    | Css_class -> Write.css_class_styler
  in

  match !mode with
  | Utf8 -> begin
    (* write utf-8 *)
    let ctx = draw_static () in
    Write.utf8 ~styler:style_fn print_string ctx
  end
  | Html -> begin
    (* write html file *)
    let ctx = draw_static () in
    if !styler = Css_class then begin
      (* write embedded css classes *)
      print_string 
        ("<html><head><meta charset=\"UTF-8\"><style>" ^ 
        Write.css_classes ^ 
        "</style></head><body><pre>\n")
    end else begin
      print_string "<html><head><meta charset=\"UTF-8\"></head><body><pre>\n";
    end;
    Write.html_escape ~styler:style_fn print_string ctx;
    print_string "</pre></body>"
  end
  | Html_scroll -> begin
    (* write html file with signals and waves as floating divs with scroll bars *)
    let sctx, wctx = draw_scroll () in
    let div_style = 
      "display:inline-block; overflow-x:auto; float:left" 
    in

    print_string ("<html><head><meta charset=\"UTF-8\"></head><body>\n");
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


