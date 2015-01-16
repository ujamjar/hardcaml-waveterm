type t = 
  {
    mutable wave_width : int;
    mutable wave_height : int;
    mutable wave_cycle : int;
    waves : Wave.t array;
  }

module Make(G : Gfx.Api) = struct

  open Gfx
  open G

  let get_wave_width = function
    | w,Wave.Clock -> w, (w+1)*2
    | w,Wave.Data _ 
    | w,Wave.Binary _ -> (w*2)+1, (w+1)*2

  let get_wave_height = function
    | 0,Wave.Clock -> 0,2
    | 0,Wave.Data _ -> 0,2
    | 0,Wave.Binary _ -> 0,2
    | 1,Wave.Clock -> 0,2
    | 1,Wave.Data _ -> 1,3
    | 1,Wave.Binary _ -> 0,2
    | h,Wave.Clock -> h-1,h+1
    | h,Wave.Data _ -> h-1,h+1
    | h,Wave.Binary _ -> h-1,h+1

  let draw_clock_cycle ~ctx ~style ~bounds ~w ~h ~c = 
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
    let rec f prev c i = 
      if i = (off+cnt) then ()
      else 
        let cur = data.(i) in
        begin match prev, cur with
        | 0, 0 -> begin
          for i=0 to w do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+i) H done
        end
        | 1, 0 -> begin
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BL;
          for i=0+1 to 0+h+1 do draw_piece ~ctx ~style ~bounds ~r:i ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c TR;
          for i=1 to w do draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c:(c+i) H done
        end
        | 0, 1 -> begin
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BR;
          for i=0+1 to 0+h+1 do draw_piece ~ctx ~style ~bounds ~r:i ~c V done;
          draw_piece ~ctx ~style ~bounds ~r:(0+h+1) ~c TL;
          for i=1 to w do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+i) H done
        end
        | 1, 1 -> begin
          for i=0 to w do draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c+i) H done
        end
        | _ -> failwith "not binary data"
        end;
        f cur (c+w+1) (i+1)
    in
    f (try data.(off-1) with _ -> data.(off)) 0 off

  let draw_data ~ctx ~style ~bounds ~w ~h ~data ~off ~cnt = 
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
      let r = 0 in
      if i = (off+cnt) then 
        (if h>0 then draw_text (r+1+((h-1)/2)) (c-prev_cnt) prev_cnt (string_of_int prev))
      else
        let cur = data.(i) in
        if prev = cur then begin
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
          (if h>0 then draw_text (r+1+((h-1)/2)) (c-prev_cnt) prev_cnt (string_of_int prev));
          f cur w (c+w+1) (i+1)
        end
    in
    f (try data.(off-1) with _ -> data.(off)) (-1) 0 off

  let rec draw_iter i bounds state f = 
    if i < Array.length state.waves && bounds.h > 0 then begin
      let _, wah = get_wave_height (state.wave_height, snd state.waves.(i)) in
      f bounds state.waves.(i);
      draw_iter (i+1) { bounds with r = bounds.r + wah; h = bounds.h - wah } state f
    end

  let draw_border ?border ~ctx ~bounds label = 
    match border with
    | None -> bounds
    | Some(style) ->
      let style = get_style style in
      G.draw_box ~ctx ~style ~bounds label;
      let bounds = { r=bounds.r+1; c=bounds.c+1; w=max 0 (bounds.w-2); h=max 0 (bounds.h-2) } in
      bounds

  let draw_wave 
    ?(style=Gfx.Style.default) ?border
    ~ctx ~bounds ~state () = 
    if bounds.w >=2 && bounds.h >= 2 then begin
      let bounds = draw_border ?border ~ctx ~bounds "Waves" in
      let style = get_style style in
      draw_iter 0 bounds state
        (fun bounds (_,wave) ->
          let wh, wah = get_wave_height (state.wave_height, wave) in
          let ww, waw = get_wave_width (state.wave_width, wave) in
          let cnt = (bounds.w + waw - 1) / waw in
          let off = state.wave_cycle in
          match wave with
          | Wave.Clock ->
            draw_clock_cycles ~ctx ~style ~bounds ~w:ww ~waw ~h:wh ~cnt 
          | Wave.Binary data ->
            let off = min (Array.length data - 1) off in
            let cnt = max 0 (min cnt (Array.length data - off)) in
            draw_binary_data ~ctx ~style ~bounds ~w:ww ~h:wh ~data ~off ~cnt
          | Wave.Data data ->
            let off = min (Array.length data - 1) off in
            let cnt = max 0 (min cnt (Array.length data - off)) in
            draw_data ~ctx ~style ~bounds ~w:ww ~h:wh ~data ~off ~cnt)
    end

  let draw_signals 
    ?(style=Gfx.Style.default) ?border
    ~ctx ~bounds ~state () = 
    if bounds.w >=2 && bounds.h >= 2 then begin
      let bounds = draw_border ?border ~ctx ~bounds "Signals" in
      let style = get_style style in
      draw_iter 0 bounds state
        (fun bounds (name,wave) ->
          let _, wah = get_wave_height (state.wave_height, wave) in
          draw_string ~ctx ~style ~bounds ~r:((wah-1)/2) ~c:0 name)
    end

  let draw_values 
    ?(style=Gfx.Style.default) ?border
    ~ctx ~bounds ~state () = 
    if bounds.w >=2 && bounds.h >= 2 then begin
      let bounds = draw_border ?border ~ctx ~bounds "Values" in
      let style = get_style style in
      draw_iter 0 bounds state
        (fun bounds (_,wave) ->
          let _, wah = get_wave_height (state.wave_height, wave) in
          match wave with
          | Wave.Clock -> ()
          | Wave.Binary d | Wave.Data d ->
            let off = state.wave_cycle in
            draw_string ~ctx ~style ~bounds ~r:((wah-1)/2) ~c:0 (string_of_int d.(off)))
    end

end

