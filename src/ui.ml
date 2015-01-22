module Make 
  (B : HardCaml.Comb.S) 
  (W : HardCamlWaveTerm.Wave.W with type elt = B.t) 
= struct

  open HardCamlWaveTerm
  open Lwt
  open LTerm_key
  open CamomileLibrary

  module G = Gfx_lterm.Api
  module R = Render.Make(G)(W)

  let rec loop ?timeout ui waves =
    let wait_ui = LTerm_ui.wait ui >>= fun ev -> Lwt.return (`event ev) in
    let sleepy time = Lwt_unix.sleep time >> Lwt.return `timeout in
    let process = function
      | `timeout -> LTerm_ui.draw ui; loop ?timeout ui waves
      | `event ev -> ui_event ?timeout ui waves ev
    in

    match timeout with
    | None -> wait_ui >>= process
    | Some(timeout) -> Lwt.pick [ sleepy timeout; wait_ui ] >>= process

  and ui_event ?timeout ui waves ev = 
      let open W in
      let draw_loop () = 
        LTerm_ui.draw ui;
        loop ?timeout ui waves
      in
      match ev with
      (* quit *)
      | LTerm_event.Key{ code = Escape } ->
        return ()
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = 'q' ->
        return ()

      (* vertical scale *)
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '+' ->
        waves.cfg.wave_height <- waves.cfg.wave_height + 1;
        draw_loop ()
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '_' ->
        waves.cfg.wave_height <- max 0 (waves.cfg.wave_height - 1);
        draw_loop ()

      (* Horizontal scale *)
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '-' ->
        waves.cfg.wave_width <- waves.cfg.wave_width - 1;
        draw_loop ()
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '=' ->
        waves.cfg.wave_width <- waves.cfg.wave_width + 1;
        draw_loop ()

      (* horizontal offset *)
      | LTerm_event.Key{ code = Home } ->
        waves.cfg.wave_cycle <- 0;
        draw_loop ()
      | LTerm_event.Key{ code = Left; shift = true } ->
        waves.cfg.wave_cycle <- max 0 (waves.cfg.wave_cycle - 10);
        draw_loop ()
      | LTerm_event.Key{ code = Left } ->
        waves.cfg.wave_cycle <- max 0 (waves.cfg.wave_cycle - 1);
        draw_loop ()
      | LTerm_event.Key{ code = End } ->
        waves.cfg.wave_cycle <- R.get_max_cycles waves - 1;
        draw_loop ()
      | LTerm_event.Key{ code = Right; shift = true } ->
        waves.cfg.wave_cycle <- min (R.get_max_cycles waves - 1) (waves.cfg.wave_cycle + 10);
        draw_loop ()
      | LTerm_event.Key{ code = Right } ->
        waves.cfg.wave_cycle <- min (R.get_max_cycles waves - 1) (waves.cfg.wave_cycle + 1);
        draw_loop ()
      (*| LTerm_event.Resize{ rows=r; cols=c } ->
        loop_wave ui*)
      | ev ->
        loop ?timeout ui waves

  let sdef = Render.Styles.colour_on_black

  let draw ?(style=sdef) ctx waves = 

    (* get bounds explictly - we may want to change them through the ui *)
    let bounds = G.get_bounds ctx in
    let bounds = Render.Bounds.fit_to_window ~status:true bounds in

    R.draw_ui ~style ~ctx ~bounds waves 

  let init ?(style=sdef) waves = 
    Lazy.force LTerm.stdout >>= fun term ->
    LTerm_ui.create term 
      (fun ui matrix -> 
        let size = LTerm_ui.size ui in
        let ctx = LTerm_draw.context matrix size in
        draw ~style ctx waves) 

  let run ?(style=sdef) ?timeout waves = 
    init ~style waves >>= fun ui ->
    (try_lwt
      loop ?timeout ui waves
    finally
      LTerm_ui.quit ui)

  let run_testbench ?(style=sdef) ?timeout waves tb = 
    let ui = run ~style ?timeout waves in
    try_lwt
      lwt tb = tb and () = ui >> (Lwt.cancel tb; Lwt.return ()) in
      Lwt.return (Some tb)
    with Lwt.Canceled ->
      Lwt.return None

end

