(* HTML or UFT-8 file generation with various options *)

type mode = Utf8 | Html | Html_scroll

type styler = No_style | Css | Css_class | Term

(* command line *)
let rows, cols = ref 0, ref 80
let width, height, start = ref 3, ref 1, ref 0
let styler = ref No_style
let scheme = ref Render.Styles.colour_on_black
let mode = ref Utf8

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
  (fun _ -> ()) "wave drawings"

module B = HardCaml.Bits.Comb.IntbitsList
module D = Wave.Bits(B)
module W = Wave.Make(D)
module G = Gfx.In_memory.Api
module R = Render.Static(W)
open Gfx
open G

let rand length bits = 
  let w = D.make () in
  for i=0 to length-1 do
    D.set w i (B.srand bits)
  done;
  w

let toggle length n bits = 
  let w = D.make () in
  for i=0 to length-1 do
    D.set w i (if (i/n) mod 2 = 0 then B.consti bits 0 else B.consti bits 1)
  done;
  w

let setone length n bits = 
  let w = D.make () in
  D.set w (length-1) (B.consti bits 0);
  D.set w n (B.consti bits 1);
  w

let waves = 
    R.R.{
      wave_width = !width;
      wave_height = !height;
      wave_cycle = !start;
      waves  = [|
        W.Clock "clock";
        (*W.Binary("a", rand 50 1);
        W.Data("b", rand 50 10, B.to_bstr);
        W.Data("c", rand 50 4, (fun s -> Printf.sprintf "%1x" (B.to_int s)));
        W.Data("data_out_port", rand 50 6, (fun s -> Printf.sprintf "%i" (B.to_sint s)));*)
        W.Binary("toggle1", toggle 50 1 1);
        W.Binary("toggle2", toggle 50 2 1);
        W.Binary("toggle3", toggle 50 3 1);
        W.Binary("toggle4", toggle 50 4 1);
        W.Data("doggle1", toggle 50 1 1, W.to_str);
        W.Data("doggle2", toggle 50 2 1, W.to_str);
        W.Data("doggle3", toggle 50 3 1, W.to_str);
        W.Data("doggle4", toggle 50 4 1, W.to_str);
        W.Data("doggle8", toggle 50 8 1, W.to_str);
        W.Data("doggle8", toggle 50 9 1, W.to_str);
        W.Data("doggle8", toggle 50 10 1, W.to_str);
        W.Data("doggle8", toggle 50 11 1, W.to_str);
        W.Data("doggle8", toggle 50 12 1, W.to_str);
        W.Data("long", setone 50 32 1, W.to_str);
      |];
    }

(* static user interface *)
let draw_static () = 
  R.draw ~style:!scheme 
    ?rows:(if !rows=0 then None else Some(!rows))
    ~cols:!cols waves

(* draw everything at full size *)
let draw_scroll () = 
  let sctx, _, wctx = R.draw_full ~style:!scheme waves in
  sctx, wctx

let () = 
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
    let ctx = draw_static () in
    Write.utf8 ~styler:style_fn print_string ctx
  end
  | Html -> begin
    (* write html file *)
    let ctx = draw_static () in
    html_header ();
    print_string "<body><pre>\n";
    Write.html_escape ~styler:style_fn print_string ctx;
    print_string "</pre></body>"
  end
  | Html_scroll -> begin
    (* write html file with signals and waves as floating divs with scroll bars.
     * bit of a hack for the width %'s.  Not really sure how its supposed to
     * work, but it seems to be OK. *)
    let sctx, wctx = draw_scroll () in
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


