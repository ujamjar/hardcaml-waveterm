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
  open W
  open Wave

  let get_wave_width (w,d) = 
    if w < 0 then 
      (* subcycle rendering *)
      match d with
      | Clock _ -> w, 1
      | Binary _ 
      | Data _ -> w, 1
    else
      match d with
      | Clock _ -> w, (w+1)*2
      | Data _ 
      | Binary _ -> (w*2)+1, (w+1)*2

  let get_wave_height = function
    | 0,Clock _ -> 0,2
    | 0,Data _ -> 0,2
    | 0,Binary _ -> 0,2
    | 1,Clock _ -> 0,2
    | 1,Data _ -> 1,3
    | 1,Binary _ -> 0,2
    | h,Clock _ -> h-1,h+1
    | h,Data _ -> h-1,h+1
    | h,Binary _ -> h-1,h+1

  let get_max_signal_width state = 
    Array.fold_left 
      (fun m s -> max m (String.length (get_name s))) 
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

  let get_w_scale w = if w < -1 then - w else 1

  let get_max_wave_width state = 
    let cycles = get_max_cycles state in
    let w, waw = get_wave_width (state.wave_width, Clock "") in
    let w_scale = get_w_scale w in
    waw * ((cycles + w_scale - 1) / w_scale)

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
    if w < 0 then begin
      for c=c to c+1 do draw_piece ~ctx ~style ~bounds ~r:0 ~c:c BH; done;
      for r=1 to h do
        for c=c to c+1 do draw_piece ~ctx ~style ~bounds ~r:r ~c:c F; done
      done;
      for c=c to c+1 do draw_piece ~ctx ~style ~bounds ~r:(h+1) ~c:c TH; done;
    end else begin
      draw_piece ~ctx ~style ~bounds ~r:0 ~c:c BR; 
      for i=0 to w-1 do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+1+i) H done;
      draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+w+1) BL;
      for i=0 to h-1 do draw_piece ~ctx ~style ~bounds ~r:(0+i+1) ~c:(c+w+1) V done;
      draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+w+1) TR;
      for i=0 to w-1 do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+w+2+i) H done;
      draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+w+w+2) TL;
      for i=0 to h-1 do draw_piece ~ctx ~style ~bounds ~r:(0+i+1) ~c:(c+w+w+2) V done
    end

  let draw_clock_cycles ~ctx ~style ~bounds ~w ~waw ~h ~cnt = 
    for i=0 to cnt - 1 do
      draw_clock_cycle ~ctx ~style ~bounds ~w ~h ~c:(i*waw)
    done

  let get_fuzzy_data data i w_scale = 
    let rec f i w_scale prev = 
      if w_scale = 0 then Some prev
      else 
        let d = W.get data i in
        if W.compare d prev then f (i+1) (w_scale-1) prev
        else None
    in
    let d = W.get data i in (* if we get 1 element, then we succeed *)
    try f (i+1) (w_scale-1) d with _ -> Some(d)

  let get_data data off i w_scale = 
    if w_scale < -1 then 
      let w_scale = get_w_scale w_scale in
      get_fuzzy_data data ((w_scale * i) + off) w_scale
    else
      Some (W.get data (off + i))

  let draw_binary_data ~ctx ~style ~bounds ~w ~h ~data ~off =  
    let open Gfx in
    let w_scale, w = w, max 0 w in
    let rec f prev c i = 
      if c >= bounds.w then ()
      else 
        let cur = get_data data off i w_scale in
        let low() = 
          for i=0 to w do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+i) H done
        in
        let low_high() = 
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BR;
          for i=0+1 to 0+h+1 do draw_piece ~ctx ~style ~bounds ~r:i ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c TL;
          for i=1 to w do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+i) H done
        in
        let high_low() = 
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BL;
          for i=0+1 to 0+h+1 do draw_piece ~ctx ~style ~bounds ~r:i ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c TR;
          for i=1 to w do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+i) H done
        in
        let high () = 
          for i=0 to w do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+i) H done
        in
        let fuzz () = 
          for c=c to c+w do draw_piece ~ctx ~style ~bounds ~r:0 ~c BH done;
          for c=c to c+w do 
            for r=1 to h do draw_piece ~ctx ~style ~bounds ~r:r ~c F done
          done;
          for c=c to c+w do draw_piece ~ctx ~style ~bounds ~r:(h+1) ~c TH done
        in
        let fuzzy p = p=None in
        let zero = function Some(p) -> W.compare p W.zero | _ -> false in
        let one = function Some(p) -> W.compare p W.one | _ -> false in
        if fuzzy cur then fuzz ()
        else if fuzzy prev && zero cur then low ()
        else if fuzzy prev && one cur then high ()
        else if zero prev && zero cur then low ()
        else if one prev && zero cur then high_low ()
        else if zero prev && one cur then low_high ()
        else if one prev && one cur then high ()
        else begin
          failwith "not binary data"
        end;
        f cur (c+w+1) (i+1)
    in
    try f None 0 0
    with _ -> ()

  let draw_data ~ctx ~style ~bounds ~to_str ~w ~h ~data ~off = 
    let w_scale, w = w, max 0 w in
    let draw_text r c cnt data = 
      match data with 
      | None -> ()
      | Some(data) ->
        let str = to_str data in
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
      if c >= bounds.w then 
        (if h>0 then draw_text (r+1+((h-1)/2)) (c-prev_cnt) prev_cnt prev)
      else
        let cur = get_data data off i w_scale in
        let fuzzy p = p=None in
        let same a b = 
          match a,b with
          | Some(a), Some(b) when W.compare a b -> true
          | _ -> false
        in
        let transn () = 
          draw_piece ~ctx ~style ~bounds ~r ~c T;
          for r=r+1 to r+h do draw_piece ~ctx ~style ~bounds ~r ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(r+h+1) ~c Tu;
          for c=c+1 to c+w do
            draw_piece ~ctx ~style ~bounds ~r ~c H;
            draw_piece ~ctx ~style ~bounds ~r:(r+h+1) ~c H;
          done;
        in
        let extend () = 
          for c=c to c+w do
            draw_piece ~ctx ~style ~bounds ~r ~c H;
            draw_piece ~ctx ~style ~bounds ~r:(r+h+1) ~c H;
          done;
        in
        let fuzz () = 
          for c=c to c+w do draw_piece ~ctx ~style ~bounds ~r:0 ~c BH done;
          for c=c to c+w do 
            for r=1 to h do draw_piece ~ctx ~style ~bounds ~r:r ~c F done
          done;
          for c=c to c+w do draw_piece ~ctx ~style ~bounds ~r:(h+1) ~c TH done;
        in
        let run fn txt ext = 
          fn ();
          (if txt && h>0 then draw_text (r+1+((h-1)/2)) (c-prev_cnt) prev_cnt prev);
          f cur (if ext then prev_cnt+w+1 else w) (c+w+1) (i+1) 
        in
        if fuzzy cur && not (fuzzy prev) then run fuzz true false
        else if fuzzy cur && fuzzy prev then run fuzz false false
        else if fuzzy prev then run extend false false 
        else if same prev cur then run extend false true
        else run transn true false 
    in
    try f None (-1) 0 0
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
          | Clock(_) ->
            draw_clock_cycles ~ctx ~style ~bounds ~w:ww ~waw ~h:wh ~cnt 
          | Binary(_, data) ->
            let off = min (W.length data - 1) off in
            draw_binary_data ~ctx ~style ~bounds ~w:ww ~h:wh ~data ~off
          | Data(_, data, to_str) ->
            let off = min (W.length data - 1) off in
            draw_data ~ctx ~style ~bounds ~to_str ~w:ww ~h:wh ~data ~off)
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
          | Clock _ -> ()
          | Binary(_, d) ->
            let off = state.wave_cycle in
            let d = try W.get d off with _ -> W.get d (W.length d - 1) in
            draw_string ~ctx ~style ~bounds ~r:((wah-1)/2) ~c:0 (W.to_str d)
          | Data(_, d, to_str) ->
            let off = state.wave_cycle in
            let d = try W.get d off with _ -> W.get d (W.length d - 1) in
            draw_string ~ctx ~style ~bounds ~r:((wah-1)/2) ~c:0 (to_str d))
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

