open Gfx
module G = Gfx.In_memory.Api
module R = Render.Make(G)

type state = 
  {
    mutable signal_window_width : int;
    mutable value_window_width : int;
    mutable waveform_window_width : int;
    wave : Render.t;
  }

let get_initial_window_sizes ui_size state = 
  if ui_size < 6 then failwith "window too small to render"
  else begin
    state.signal_window_width <- max 2 (ui_size / 6);
    state.value_window_width <- max 2 (ui_size / 6);
    state.waveform_window_width <- 
      ui_size - state.signal_window_width - state.value_window_width;
  end

let draw_ui ctx state = 
  let open G in
  let open Gfx.Style in

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

let main () = 
  let rows, cols = 31, 100 in
  let ctx = Gfx.In_memory.init ~rows ~cols in
  let state = 
    {
      signal_window_width = 15;
      value_window_width = 0;
      waveform_window_width = cols-15;
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
  in
  draw_ui ctx state;

  let os = print_string in
  let gen_html = false in
  if gen_html then begin
    os "<html><head><meta charset=\"UTF-8\"></head><body><pre>\n";
    Gfx_html.write_html_escape ~styler:Gfx_html.html_styler os ctx;
    os "</pre></body>"
  end else
    Gfx_html.write_utf8 ~styler:Gfx_html.term_styler os ctx

let () = main()

