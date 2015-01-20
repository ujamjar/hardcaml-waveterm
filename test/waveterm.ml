open Lwt
open LTerm_geom
open LTerm_key
open CamomileLibrary

open HardCamlWaveTerm
open HardCamlWaveLTerm

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

module B = HardCaml.Bits.Comb.IntbitsList
module D = Wave.Bits(B)
module W = Wave.Make(D)
module G = Gfx_lterm.Api
module R = Render.Make(G)(W)
open Gfx
open G

let rand length bits = 
  let module B = HardCaml.Bits.Comb.IntbitsList in
  let w = D.make () in
  for i=0 to length-1 do
    D.set w i (B.srand bits)
  done;
  w

let get_waves wave_width wave_height = 
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

let cycles waves = R.get_max_cycles waves

let rec loop_wave ui waves =
  let open R in
  let draw_loop () = 
    LTerm_ui.draw ui;
    loop_wave ui waves
  in
  LTerm_ui.wait ui >>= function
    (* quit *)
    | LTerm_event.Key{ code = Escape } ->
        return ()
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = 'q' ->
        return ()

    (* vertical scale *)
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '+' ->
        waves.wave_height <- waves.wave_height + 1;
        draw_loop ()
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '_' ->
        waves.wave_height <- max 0 (waves.wave_height - 1);
        draw_loop ()

    (* Horizontal scale *)
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '-' ->
        waves.wave_width <- max 0 (waves.wave_width - 1);
        draw_loop ()
    | LTerm_event.Key{ code = Char c } when UChar.char_of c = '=' ->
        waves.wave_width <- waves.wave_width + 1;
        draw_loop ()

    (* horizontal offset *)
    | LTerm_event.Key{ code = Home } ->
        waves.wave_cycle <- 0;
        draw_loop ()
    | LTerm_event.Key{ code = Left; shift = true } ->
        waves.wave_cycle <- max 0 (waves.wave_cycle - 10);
        draw_loop ()
    | LTerm_event.Key{ code = Left } ->
        waves.wave_cycle <- max 0 (waves.wave_cycle - 1);
        draw_loop ()
    | LTerm_event.Key{ code = End } ->
        waves.wave_cycle <- cycles waves - 1;
        draw_loop ()
    | LTerm_event.Key{ code = Right; shift = true } ->
        waves.wave_cycle <- min (cycles waves - 1) (waves.wave_cycle + 10);
        draw_loop ()
    | LTerm_event.Key{ code = Right } ->
        waves.wave_cycle <- min (cycles waves - 1) (waves.wave_cycle + 1);
        draw_loop ()
    (*| LTerm_event.Resize{ rows=r; cols=c } ->
        loop_wave ui*)
    | ev ->
        loop_wave ui waves

(* set up the user interface *)
let draw ctx waves = 

  let style = Render.Styles.colour_on_black in

  (* get bounds explictly - we may want to change them through the ui *)
  let bounds = get_bounds ctx in
  let bounds = Render.Bounds.fit_to_window bounds in 

  R.draw_ui ~style ~ctx ~bounds waves 

let run_wave () = 
  let waves = get_waves 3 1 in
  lwt term = Lazy.force LTerm.stdout in
  lwt ui = LTerm_ui.create term (fun ui matrix -> 
    let size = LTerm_ui.size ui in
    let ctx = LTerm_draw.context matrix size in
    draw ctx waves) 
  in
  try_lwt
    loop_wave ui waves
  finally
    LTerm_ui.quit ui

lwt () = run_wave ()


