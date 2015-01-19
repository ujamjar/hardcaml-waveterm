open Lwt
open LTerm_geom
open LTerm_key
open CamomileLibrary

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

module W = Wave.Make(Wave.Bits)
module G = Gfx_lterm.Api
module R = Render.Make(G)(W)
open Gfx
open G

type state = 
  {
    mutable signal_window_width : int;
    mutable value_window_width : int;
    mutable waveform_window_width : int;
    wave : R.t;
  }

let rand length bits = 
  let module B = HardCaml.Bits.Comb.IntbitsList in
  let w = Wave.Bits.make () in
  for i=0 to length-1 do
    Wave.Bits.set w i (B.srand bits)
  done;
  w

let get_state cols wave_width wave_height = 
  let module B = HardCaml.Bits.Comb.IntbitsList in
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
          W.Clock "clock";
          W.Binary("a", rand 50 1);
          W.Data("b", rand 50 10, B.to_bstr);
          W.Data("c", rand 50 4, (fun s -> Printf.sprintf "%1x" (B.to_int s)));
          W.Data("data_out_port", rand 50 6, (fun s -> Printf.sprintf "%i" (B.to_sint s)));
        |];
      }
  }

let cycles state = R.get_max_cycles state.wave

let rec loop_wave ui state =
  let open R in
  let draw_loop () = 
    LTerm_ui.draw ui;
    loop_wave ui state
  in
  LTerm_ui.wait ui >>= function
    (* quit *)
    | LTerm_event.Key{ code = Escape } ->
        return ()
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = 'q' ->
        return ()

    (* vertical scale *)
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '+' ->
        state.wave.wave_height <- state.wave.wave_height + 1;
        draw_loop ()
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '_' ->
        state.wave.wave_height <- max 0 (state.wave.wave_height - 1);
        draw_loop ()

    (* Horizontal scale *)
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '-' ->
        state.wave.wave_width <- max 0 (state.wave.wave_width - 1);
        draw_loop ()
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '=' ->
        state.wave.wave_width <- state.wave.wave_width + 1;
        draw_loop ()

    (* horizontal offset *)
    | LTerm_event.Key{ code = Home } ->
        state.wave.wave_cycle <- 0;
        draw_loop ()
    | LTerm_event.Key{ code = Left; shift = true } ->
        state.wave.wave_cycle <- max 0 (state.wave.wave_cycle - 10);
        draw_loop ()
    | LTerm_event.Key{ code = Left } ->
        state.wave.wave_cycle <- max 0 (state.wave.wave_cycle - 1);
        draw_loop ()
    | LTerm_event.Key{ code = End } ->
        state.wave.wave_cycle <- cycles state - 1;
        draw_loop ()
    | LTerm_event.Key{ code = Right; shift = true } ->
        state.wave.wave_cycle <- min (cycles state - 1) (state.wave.wave_cycle + 10);
        draw_loop ()
    | LTerm_event.Key{ code = Right } ->
        state.wave.wave_cycle <- min (cycles state - 1) (state.wave.wave_cycle + 1);
        draw_loop ()
    (*| LTerm_event.Resize{ rows=r; cols=c } ->
        loop_wave ui*)
    | ev ->
        loop_wave ui state

let get_initial_window_sizes ui_size state = 
  if ui_size < 6 then failwith "window too small to render"
  else begin
    state.signal_window_width <- max 2 (ui_size / 6);
    state.value_window_width <- max 2 (ui_size / 6);
    state.waveform_window_width <- 
      ui_size - state.signal_window_width - state.value_window_width;
  end

(* set up the user interface *)
let draw ctx state = 

  let bounds = get_bounds ctx in

  let style, border, (sstyle, vstyle, wstyle) = get_styling Colour_on_black in

  let sbounds = { r=0; c=0; w=state.signal_window_width; h=bounds.h } in
  let vbounds = { r=0; c=sbounds.c+sbounds.w; w=state.value_window_width; h=bounds.h } in
  let wbounds = { r=0; c=vbounds.c+vbounds.w; w=state.waveform_window_width; h=bounds.h } in

  R.draw_ui
    ~style ~sstyle ~vstyle ~wstyle ~border
    ~ctx ~sbounds ~vbounds ~wbounds ~state:state.wave ()

let run_wave () = 
  let state = get_state 80 3 1 in
  lwt term = Lazy.force LTerm.stdout in
  lwt ui = LTerm_ui.create term (fun ui matrix -> 
    let size = LTerm_ui.size ui in
    let ctx = LTerm_draw.context matrix size in
    draw ctx state) 
  in
  let size = LTerm_ui.size ui in
  get_initial_window_sizes size.cols state;
  try_lwt
    loop_wave ui state
  finally
    LTerm_ui.quit ui

lwt () = run_wave ()


