module Make (G : Gfx.Api) = struct

  module R = Render.Make(G)(Wave.Int)

  (* some example data *)
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

  open G
  open Gfx
  open Gfx.Style

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

end

