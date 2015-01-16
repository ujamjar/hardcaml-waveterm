open Lwt
open LTerm_geom
open LTerm_key
open CamomileLibrary

open Gfx
module G = Gfx_lterm.Api
module R = Render.Make(G)

type state = 
  {
    mutable signal_window_width : int;
    mutable value_window_width : int;
    mutable waveform_window_width : int;
    wave : Render.t;
  }

let draw_cursor ~matrix ~ctx ~bounds n = 
  LTerm_draw.set_style matrix.(20).(20) LTerm_style.{ none with background = Some red }

let draw_ui ui matrix state = 
  let open Gfx_lterm.Api in
  let open Gfx.Style in

  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in

  let bounds = get_bounds ctx in
  let style = default in
  fill ~ctx ~style:(get_style style) ~bounds ' ';

  let sbox = { r=0; c=0; w=state.signal_window_width; h=bounds.h } in
  let vbox = { r=0; c=sbox.c+sbox.w; w=state.value_window_width; h=bounds.h } in
  let wbox = { r=0; c=vbox.c+vbox.w; w=state.waveform_window_width; h=bounds.h } in

  let border = style in
  R.draw_signals ~style:{style with fg=Blue} ~border ~ctx ~bounds:sbox ~state:state.wave ();
  R.draw_values ~style:{style with fg=Yellow} ~border ~ctx ~bounds:vbox ~state:state.wave ();
  R.draw_wave ~style:{style with fg=Green} ~border ~ctx ~bounds:wbox ~state:state.wave ()

let cycles state = 
  Array.fold_left (fun m (_,w) -> 
    match w with
    | Wave.Clock -> m
    | Wave.Binary d | Wave.Data d -> max m (Array.length d)) 0 state.wave.Render.waves

let rec loop_wave ui state =
  let open Render in
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

let run_wave () = 
  let state = 
    {
      signal_window_width = 0;
      value_window_width = 0;
      waveform_window_width = 0;
      wave = 
        Render.{
          wave_width = 3;
          wave_height = 1;
          wave_cycle = 0;
          waves  = [|
            "clock", Wave.Clock;
            "a", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
            "b", Wave.Data(Array.init 50 (fun _ -> Random.int 1000-500));
            "c", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
            "b", Wave.Data(Array.init 50 (fun _ -> Random.int 10));
            "b", Wave.Data(Array.init 50 (fun _ -> Random.int 3));
            "d", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
            "e", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
            "b", Wave.Data(Array.init 50 (fun _ -> Random.int 10-5));
            "clock2", Wave.Clock;
            "b", Wave.Data(Array.init 50 (fun _ -> Random.int 20-10));
            "f", Wave.Binary(Array.init 50 (fun _ -> Random.int 2));
          |];
        }
    }
  in
  lwt term = Lazy.force LTerm.stdout in
  (*lwt ui = LTerm_ui.create term (fun ui matrix -> draw_wave_test ui matrix) in*)
  lwt ui = LTerm_ui.create term (fun ui matrix -> draw_ui ui matrix state) in
  let size = LTerm_ui.size ui in
  get_initial_window_sizes size.cols state;
  try_lwt
    loop_wave ui state
  finally
    LTerm_ui.quit ui

lwt () = run_wave ()


