(* some example data *)
type state = 
  {
    mutable signal_window_width : int;
    mutable value_window_width : int;
    mutable waveform_window_width : int;
    wave : Render.t;
  }

let state cols wave_width wave_height = 
  Gfx.{
    signal_window_width = 10;
    value_window_width = 10;
    waveform_window_width = cols-20;
    wave = 
      Render.{
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

let rows, cols = ref 31, ref 80
let width, height = ref 3, ref 1
let styler = ref Write.html_styler

let parse_args () = Arg.parse
  [
    "-rows", Arg.Set_int rows, "number of rows";
    "-cols", Arg.Set_int cols, "number of cols";
    "-width", Arg.Set_int width, "wave width";
    "-height", Arg.Set_int height, "wave height ";
    "-nostyle", Arg.Unit(fun () -> styler := Write.no_styler), "disable styles";
  ]
  (fun _ -> ()) "wave viewer"

module Ui(G : Gfx.Api) = struct

  open G
  open Gfx
  open Gfx.Style
  module R = Render.Make(G)

  (* set up the user interface *)
  let draw ctx state = 

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

end
