module Styles = struct
  type t =
    {
      style : Gfx.Style.t;
      border : Gfx.Style.t option;
      signals : Gfx.Style.t;
      values : Gfx.Style.t;
      waves : Gfx.Style.t;
    }
  let default d = { style = d; border = Some(d); signals = d; values = d; waves = d; }
  let white_on_black = default Gfx.Style.{ default with fg=White; bg=Black }
  let black_on_white = default Gfx.Style.{ default with fg=Black; bg=White }
  let colour s = 
    { s with 
      signals = Gfx.Style.{ s.signals with fg = Blue };
      values = Gfx.Style.{ s.values with fg = Red };
      waves = Gfx.Style.{ s.waves with fg = Green };
    }
  let colour_on_black = colour white_on_black
  let colour_on_white = colour black_on_white
end

module Bounds = struct

  type t = 
    {
      signals : Gfx.rect;
      values : Gfx.rect;
      waves : Gfx.rect;
    }

  let fit_to_window ?(signals=true) ?(values=true) ?(waves=true) bounds = 
    let open Gfx in
    let rows, cols = bounds.h, bounds.w in
    let iw6 = max 3 (min 20 (cols / 6)) in (* approx 1/6 of width, >3 and < 20 *)
    let iw4 = max 3 (min 20 (cols / 4)) in (* approx 1/4 of width, >3 and < 20 *)
    let z = { r=0; c=0; w=0; h=rows } in
    let get_bounds w0 w1 w2 = 
      if w2 <= 0 && waves then failwith "windows wont fit (sorry, should be more graceful!)"
      else 
        { 
          signals={ z with w=w0 }; 
          values={ z with c=w0; w=w1 }; 
          waves={ z with c=w0+w1; w=w2 } 
        }
    in
    match signals, values, waves with
    (* all *)
    | true, true, true -> get_bounds iw6 iw6 (cols - iw6 - iw6)
    (* 2 *)
    | true, true, false -> get_bounds (cols/2) ((cols+1)/2) 0
    | true, false, true -> get_bounds iw4 0 (cols-iw4)
    | false, true, true -> get_bounds 0 iw4 (cols-iw4)
    (* 1 *)
    | true, false, false -> get_bounds cols 0 0
    | false, true, false -> get_bounds 0 cols 0
    | false, false, true -> get_bounds 0 0 cols
    (* 0 *)
    | false, false, false -> get_bounds 0 0 0


end

module Make(G : Gfx.Api) (W : Wave.W) = struct

  open G

  type t = 
    {
      (* width of a cycle *)
      mutable wave_width : int;
      (* height of a cycle *)
      mutable wave_height : int;
      (* starting cycle *)
      mutable wave_cycle : int;
      (* data *)
      waves : W.wave array;
    }

  let get_wave_width = function
    | w,W.Clock _ -> w, (w+1)*2
    | w,W.Data _ 
    | w,W.Binary _ -> (w*2)+1, (w+1)*2

  let get_wave_height = function
    | 0,W.Clock _ -> 0,2
    | 0,W.Data _ -> 0,2
    | 0,W.Binary _ -> 0,2
    | 1,W.Clock _ -> 0,2
    | 1,W.Data _ -> 1,3
    | 1,W.Binary _ -> 0,2
    | h,W.Clock _ -> h-1,h+1
    | h,W.Data _ -> h-1,h+1
    | h,W.Binary _ -> h-1,h+1

  let get_max_signal_width state = 
    Array.fold_left 
      (fun m s -> max m (String.length (W.get_name s))) 
      0 state.waves 

  let get_max_value_width state = 
    let fold f a d = 
      let len = W.length d in
      let rec g a i = if i=len then a else g (f a (W.get d i)) (i+1) in
      g a 0
    in
    Array.fold_left 
      (fun m w -> 
        try
          let data = W.get_data w in
          let to_str = W.get_to_str w in 
          let max m s = max m (String.length (to_str s)) in
          fold max m data
        with _ -> m)
      0 state.waves

  let get_max_cycles state = 
    Array.fold_left 
      (fun m d -> 
        max m (try W.length (W.get_data d) with _ -> 0))
      0 state.waves

  let get_max_wave_width state = 
    let cycles = get_max_cycles state in
    let _, waw = get_wave_width (state.wave_width, W.Clock "") in
    waw * cycles

  let get_max_wave_height state = 
    Array.fold_left
      (fun a d ->
        let _, wah = get_wave_height (state.wave_height, d) in
        a + wah) 
      0 state.waves

  let get_max_bounds state = 
    let open Gfx in
    let swidth = get_max_signal_width state in
    let vwidth = get_max_value_width state in
    let wwidth = get_max_wave_width state in
    let wheight = get_max_wave_height state in
    let z = { r=0; c=0; h=wheight; w=0 } in
    Bounds.({
      signals = {z with w = swidth};
      values = {z with w = vwidth};
      waves = {z with w = wwidth};
    })

  let draw_clock_cycle ~ctx ~style ~bounds ~w ~h ~c = 
    let open Gfx in
    draw_piece ~ctx ~style ~bounds ~r:0 ~c:c BR; 
    for i=0 to w-1 do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+1+i) H done;
    draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+w+1) BL;
    for i=0 to h-1 do draw_piece ~ctx ~style ~bounds ~r:(0+i+1) ~c:(c+w+1) V done;
    draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+w+1) TR;
    for i=0 to w-1 do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+w+2+i) H done;
    draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+w+w+2) TL;
    for i=0 to h-1 do draw_piece ~ctx ~style ~bounds ~r:(0+i+1) ~c:(c+w+w+2) V done

  let draw_clock_cycles ~ctx ~style ~bounds ~w ~waw ~h ~cnt = 
    for i=0 to cnt - 1 do
      draw_clock_cycle ~ctx ~style ~bounds ~w ~h ~c:(i*waw)
    done

  let draw_binary_data ~ctx ~style ~bounds ~w ~h ~data ~off ~cnt =  
    let open Gfx in
    let rec f prev c i = 
      if i = (off+cnt) then ()
      else 
        let cur = W.get data i in
        if W.(compare prev zero && compare cur zero) then begin
          for i=0 to w do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+i) H done
        end else if W.(compare prev one && compare cur zero) then begin
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BL;
          for i=0+1 to 0+h+1 do draw_piece ~ctx ~style ~bounds ~r:i ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c TR;
          for i=1 to w do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+i) H done
        end else if W.(compare prev zero && compare cur one) then begin
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BR;
          for i=0+1 to 0+h+1 do draw_piece ~ctx ~style ~bounds ~r:i ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c TL;
          for i=1 to w do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+i) H done
        end else if W.(compare prev one && compare cur one) then begin
          for i=0 to w do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+i) H done
        end else begin
          failwith "not binary data"
        end;
        f cur (c+w+1) (i+1)
    in
    try f (try W.get data (off-1) with _ -> W.get data off) 0 off
    with _ -> ()

  let draw_data ~ctx ~style ~bounds ~to_str ~w ~h ~data ~off ~cnt = 
    let draw_text r c cnt str = 
      let putc i ch = draw_char ~ctx ~style ~bounds ~r ~c:(c+i) ch in
      let str_len = String.length str in
      if str_len <= cnt then 
        for i=0 to str_len-1 do
          putc i str.[i]
        done
      else
        for i=0 to cnt-1 do
          putc i (if i=(cnt-1) then '.' else str.[i])
        done
    in
    let rec f prev prev_cnt c i = 
      let open Gfx in
      let r = 0 in
      if i = (off+cnt) then 
        (if h>0 then draw_text (r+1+((h-1)/2)) (c-prev_cnt) prev_cnt (to_str prev))
      else
        let cur = W.get data i in
        if W.compare prev cur then begin
          for c=c to c+w do
            draw_piece ~ctx ~style ~bounds ~r ~c H;
            draw_piece ~ctx ~style ~bounds ~r:(r+h+1) ~c H;
          done;
          f cur (prev_cnt+w+1) (c+w+1) (i+1)
        end else begin
          draw_piece ~ctx ~style ~bounds ~r ~c T;
          for r=r+1 to r+h do draw_piece ~ctx ~style ~bounds ~r ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(r+h+1) ~c Tu;
          for c=c+1 to c+w do
            draw_piece ~ctx ~style ~bounds ~r ~c H;
            draw_piece ~ctx ~style ~bounds ~r:(r+h+1) ~c H;
          done;
          (if h>0 then draw_text (r+1+((h-1)/2)) (c-prev_cnt) prev_cnt (to_str prev));
          f cur w (c+w+1) (i+1)
        end
    in
    try f (try W.get data (off-1) with _ -> W.get data off) (-1) 0 off
    with _ -> ()

  let rec draw_iter i bounds state f = 
    let open Gfx in
    if i < Array.length state.waves && bounds.h > 0 then begin
      let _, wah = get_wave_height (state.wave_height, state.waves.(i)) in
      f bounds state.waves.(i);
      draw_iter (i+1) { bounds with r = bounds.r + wah; h = bounds.h - wah } state f
    end

  let draw_border ?border ~ctx ~bounds label = 
    let open Gfx in
    match border with
    | None -> bounds
    | Some(style) ->
      let style = get_style style in
      G.draw_box ~ctx ~style ~bounds label;
      let bounds = { r=bounds.r+1; c=bounds.c+1; w=max 0 (bounds.w-2); h=max 0 (bounds.h-2) } in
      bounds

  let draw_wave 
    ?(style=Gfx.Style.default) ?border
    ~ctx ~bounds state = 
    let open Gfx in
    if bounds.w >=2 && bounds.h >= 2 then begin
      let bounds = draw_border ?border ~ctx ~bounds "Waves" in
      let style = get_style style in
      draw_iter 0 bounds state
        (fun bounds wave ->
          let wh, wah = get_wave_height (state.wave_height, wave) in
          let ww, waw = get_wave_width (state.wave_width, wave) in
          let cnt = (bounds.w + waw - 1) / waw in
          let off = state.wave_cycle in
          match wave with
          | W.Clock(_) ->
            draw_clock_cycles ~ctx ~style ~bounds ~w:ww ~waw ~h:wh ~cnt 
          | W.Binary(_, data) ->
            let off = min (W.length data - 1) off in
            let cnt = max 0 (min cnt (W.length data - off)) in
            draw_binary_data ~ctx ~style ~bounds ~w:ww ~h:wh ~data ~off ~cnt
          | W.Data(_, data, to_str) ->
            let off = min (W.length data - 1) off in
            let cnt = max 0 (min cnt (W.length data - off)) in
            draw_data ~ctx ~style ~bounds ~to_str ~w:ww ~h:wh ~data ~off ~cnt)
    end

  let draw_signals 
    ?(style=Gfx.Style.default) ?border
    ~ctx ~bounds state = 
    let open Gfx in
    if bounds.w >=2 && bounds.h >= 2 then begin
      let bounds = draw_border ?border ~ctx ~bounds "Signals" in
      let style = get_style style in
      draw_iter 0 bounds state
        (fun bounds wave ->
          let _, wah = get_wave_height (state.wave_height, wave) in
          draw_string ~ctx ~style ~bounds ~r:((wah-1)/2) ~c:0 (W.get_name wave))
    end

  let draw_values 
    ?(style=Gfx.Style.default) ?border
    ~ctx ~bounds state = 
    let open Gfx in
    if bounds.w >=2 && bounds.h >= 2 then begin
      let bounds = draw_border ?border ~ctx ~bounds "Values" in
      let style = get_style style in
      draw_iter 0 bounds state
        (fun bounds wave ->
          let _, wah = get_wave_height (state.wave_height, wave) in
          match wave with
          | W.Clock _ -> ()
          | W.Binary(_, d) ->
            let off = state.wave_cycle in
            draw_string ~ctx ~style ~bounds ~r:((wah-1)/2) ~c:0 (W.to_str (W.get d off))
          | W.Data(_, d, to_str) ->
            let off = state.wave_cycle in
            draw_string ~ctx ~style ~bounds ~r:((wah-1)/2) ~c:0 (to_str (W.get d off)))
    end

  let draw_ui
    ?(style=Styles.default Gfx.Style.default) ?bounds 
    ~ctx state = 
    let open Styles in
    let open Bounds in

    let cbounds = get_bounds ctx in
    fill ~ctx ~style:(get_style style.style) ~bounds:cbounds ' ';

    let bounds = 
      match bounds with 
      | None -> fit_to_window cbounds
      | Some(b) -> b 
    in

    draw_signals ~style:style.signals ?border:style.border ~ctx ~bounds:bounds.signals state;
    draw_values ~style:style.values ?border:style.border ~ctx ~bounds:bounds.values state;
    draw_wave ~style:style.waves ?border:style.border ~ctx ~bounds:bounds.waves state

end

module Static(W : Wave.W) = struct
  module R = Make(Gfx.In_memory.Api)(W)

  let border_ext = function None -> 0 | Some _ -> 2

  let get_max_height border state = 
    border_ext border + R.get_max_wave_height state

  let draw 
    ?signals ?values ?waves ?(style=Styles.default Gfx.Style.default)
    ?rows ?cols state = 
    (* inferred width and height *)
    let cols = match cols with None -> 80 | Some(x) -> x in
    let rows = match rows with None -> get_max_height style.Styles.border state | Some(x) -> x in

    (* do drawing *)
    let ctx = Gfx.In_memory.init ~rows ~cols in
    let bounds = Bounds.fit_to_window ?signals ?values ?waves Gfx.({r=0; c=0; h=rows; w=cols}) in
    R.draw_ui ~style ~ctx ~bounds state;
    
    (* return context *) 
    ctx

  let draw_full ?(style=Styles.default Gfx.Style.default) state = 
    let open Bounds in
    let open Styles in

    let bounds = R.get_max_bounds state in
    let ext = border_ext style.border in
    let get_ctx b = 
      let open Gfx in
      let b = { b with w = b.w + ext; h = b.h + ext } in
      let ctx = Gfx.In_memory.init ~rows:b.h ~cols:b.w in
      Gfx.In_memory.Api.(fill ~ctx ~style:(get_style style.style) ~bounds:b ' ');
      b, ctx
    in

    let b, sctx = get_ctx bounds.signals in
    R.draw_signals ~style:style.signals ?border:style.border ~ctx:sctx ~bounds:b state;

    let b, vctx = get_ctx bounds.values in
    R.draw_values ~style:style.values ?border:style.border ~ctx:vctx ~bounds:b state;

    let b, wctx = get_ctx bounds.waves in
    R.draw_wave ~style:style.waves ?border:style.border ~ctx:wctx ~bounds:b state;

    sctx, vctx, wctx

end

