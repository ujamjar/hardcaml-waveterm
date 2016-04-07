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

  let show_status = true

  type state = 
    {
      mutable bounds : Render.Bounds.t;
      waves : W.waves;
    }

  let rec loop ?timeout (ui,state) =
    let wait_ui = LTerm_ui.wait ui >>= fun ev -> Lwt.return (`event ev) in
    let sleepy time = Lwt_unix.sleep time >> Lwt.return `timeout in
    let process = function
      | `timeout -> LTerm_ui.draw ui; loop ?timeout (ui, state)
      | `event ev -> ui_event ?timeout (ui,state) ev
    in

    match timeout with
    | None -> wait_ui >>= process
    | Some(timeout) -> Lwt.pick [ sleepy timeout; wait_ui ] >>= process

  and ui_event ?timeout (ui, state) ev = 
      let waves = state.waves in
      let open W in
      let draw_loop () = 
        LTerm_ui.draw ui;
        loop ?timeout (ui,state) 
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

      (* cycle offset *)
      | LTerm_event.Key{ code = Home } ->
        waves.cfg.start_cycle <- 0;
        draw_loop ()
      | LTerm_event.Key{ code = End } ->
        waves.cfg.start_cycle <- R.get_max_cycles waves - 1;
        draw_loop ()
      | LTerm_event.Key{ code = Left; shift = true; control = false; meta = false } ->
        waves.cfg.start_cycle <- max 0 (waves.cfg.start_cycle - 10);
        draw_loop ()
      | LTerm_event.Key{ code = Left; shift = false; control = false; meta = false } ->
        waves.cfg.start_cycle <- max 0 (waves.cfg.start_cycle - 1);
        draw_loop ()
      | LTerm_event.Key{ code = Right; shift = true; control = false; meta = false } ->
        waves.cfg.start_cycle <- min (R.get_max_cycles waves - 1) 
                                     (waves.cfg.start_cycle + 10);
        draw_loop ()
      | LTerm_event.Key{ code = Right; shift = false; control = false; meta = false } ->
        waves.cfg.start_cycle <- min (R.get_max_cycles waves - 1) 
                                     (waves.cfg.start_cycle + 1);
        draw_loop ()

      (* signal offset *)
      | LTerm_event.Key{ code = Up; shift = true } ->
        waves.cfg.start_signal <- max 0 (waves.cfg.start_signal - 10);
        draw_loop ()
      | LTerm_event.Key{ code = Up } ->
        waves.cfg.start_signal <- max 0 (waves.cfg.start_signal - 1);
        draw_loop ()
      | LTerm_event.Key{ code = Down; shift = true } ->
        waves.cfg.start_signal <- min (R.get_max_signals waves - 1) 
                                      (waves.cfg.start_signal + 10);
        draw_loop ()
      | LTerm_event.Key{ code = Down } ->
        waves.cfg.start_signal <- min (R.get_max_signals waves - 1) 
                                      (waves.cfg.start_signal + 1);
        draw_loop ()

      (* signal/value window scroll *)
      | LTerm_event.Key{ code = Left; shift = false; control = true; meta = false } ->
        waves.cfg.signal_scroll <- max 0 (waves.cfg.signal_scroll - 1);
        draw_loop ()
      | LTerm_event.Key{ code = Right; shift = false; control = true; meta = false } ->
        waves.cfg.signal_scroll <- waves.cfg.signal_scroll + 1;
        draw_loop ()

      | LTerm_event.Key{ code = Left; shift = false; control = false; meta = true } ->
        waves.cfg.value_scroll <- max 0 (waves.cfg.value_scroll - 1);
        draw_loop ()
      | LTerm_event.Key{ code = Right; shift = false; control = false; meta = true } ->
        waves.cfg.value_scroll <- waves.cfg.value_scroll + 1;
        draw_loop ()

      (* terminal resize *)
      | LTerm_event.Resize size ->
        let bounds = Gfx.({r=0; c=0; w=size.LTerm_geom.cols; h=size.LTerm_geom.rows}) in
        state.bounds <- Render.Bounds.fit_to_window ~status:show_status bounds;
        draw_loop ()

      (* mouse event *)
      | LTerm_event.Mouse m when LTerm_mouse.(m.button = Button1 && m.control) -> begin
        let open LTerm_mouse in
        begin
          match R.pick ~bounds:state.bounds ~r:m.row ~c:m.col waves with
          | R.Wave(cycle,signal) -> state.waves.cfg.start_cycle <- cycle;
          | R.Signal(signal) | R.Value(signal) -> state.waves.cfg.start_signal <- signal
          | _ -> ()
        end;
        draw_loop ()
      end

      | LTerm_event.Mouse m when LTerm_mouse.(m.button = Button1) -> begin
        let open LTerm_mouse in
        begin
          match R.pick ~bounds:state.bounds ~r:m.row ~c:m.col waves with
          | R.Wave(cycle,signal) -> state.waves.cfg.wave_cursor <- cycle
          | R.Signal(signal) | R.Value(signal) -> state.waves.cfg.signal_cursor <- signal
          | _ -> ()
        end;
        draw_loop ()
      end

      | ev ->
        loop ?timeout (ui, state)

  let sdef = Render.Styles.colour_on_black

  let init_state term waves = 
    let size = LTerm.size term in
    let bounds = Gfx.({r=0; c=0; w=size.LTerm_geom.cols; h=size.LTerm_geom.rows}) in
    let bounds = Render.Bounds.fit_to_window ~status:show_status bounds in
    Lwt.return
      {
        bounds = bounds;
        waves = waves;
      }

  let draw style state ui matrix = 
    let size = LTerm_ui.size ui in
    let ctx = LTerm_draw.context matrix size in
    R.draw_ui ~style ~ctx ~bounds:state.bounds state.waves

  let init ?(style=sdef) waves = 
    Lazy.force LTerm.stdout >>= fun term ->
    LTerm.enable_mouse term >>
    (* initialization *)
    init_state term waves >>= fun state ->
    (* drawing functon *)
    LTerm_ui.create term (draw style state) >>= fun ui ->
    Lwt.return (ui,state,term)

  let run ?(style=sdef) ?timeout waves = 
    init ~style waves >>= fun (ui,state,term) ->
    (try_lwt
      loop ?timeout (ui,state) 
    finally
      LTerm.disable_mouse term >>
      LTerm_ui.quit ui)

  let run_testbench ?(style=sdef) ?timeout waves tb = 
    let ui = run ~style ?timeout waves in
    try_lwt
      lwt tb = tb and () = ui >> (Lwt.cancel tb; Lwt.return ()) in
      Lwt.return (Some tb)
    with Lwt.Canceled ->
      Lwt.return None

end

