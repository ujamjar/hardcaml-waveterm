module Make (B : HardCaml.Comb.S) = struct

  open HardCamlWaveTerm
  open Lwt
  open LTerm_key
  open CamomileLibrary

  module D = Wave.Make_dynamic(Wave.Bits(B))
  module W = Wave.Make(D)
  module G = Gfx_lterm.Api
  module R = Render.Make(G)(W)

  let rec loop ui waves =
    let open R in
    let draw_loop () = 
      LTerm_ui.draw ui;
      loop ui waves
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
          waves.wave_width <- waves.wave_width - 1;
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
          waves.wave_cycle <- R.get_max_cycles waves - 1;
          draw_loop ()
      | LTerm_event.Key{ code = Right; shift = true } ->
          waves.wave_cycle <- min (R.get_max_cycles waves - 1) (waves.wave_cycle + 10);
          draw_loop ()
      | LTerm_event.Key{ code = Right } ->
          waves.wave_cycle <- min (R.get_max_cycles waves - 1) (waves.wave_cycle + 1);
          draw_loop ()
      (*| LTerm_event.Resize{ rows=r; cols=c } ->
          loop_wave ui*)
      | ev ->
          loop ui waves

  let loop ui waves =
    try_lwt
      loop ui waves
    finally
      LTerm_ui.quit ui

  let draw ctx waves = 

    let style = Render.Styles.colour_on_black in

    (* get bounds explictly - we may want to change them through the ui *)
    let bounds = G.get_bounds ctx in
    let bounds = Render.Bounds.fit_to_window bounds in 

    R.draw_ui ~style ~ctx ~bounds waves 

  let init waves = 
    Lazy.force LTerm.stdout >>= fun term ->
    LTerm_ui.create term 
      (fun ui matrix -> 
        let size = LTerm_ui.size ui in
        let ctx = LTerm_draw.context matrix size in
        draw ctx waves) 

end

