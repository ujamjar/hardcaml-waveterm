type styler = 
  {
    start : (string -> unit) -> unit;
    set : (string -> unit) -> Gfx.Style.t -> unit;
    eol : (string -> unit) -> unit;
    finish : (string -> unit) -> unit;
  }

let html_styler = 
  let open Gfx.Style in
  let get_colour = function
    | Black -> "black" | Red -> "red" | Green -> "green" | Yellow -> "yellow"
    | Blue -> "blue" | Magenta -> "magenta" | Cyan -> "cyan" | White -> "white"
  in
  let prev = ref default in
  let set_style style os = 
    os (Printf.sprintf "<span style=\"background-color:%s; color:%s; font-wieght:%s\">"
      (get_colour style.bg) (get_colour style.fg) (if style.bold then "bold" else "normal"))
  in
  let close_style os = os "</span>" in
  { 
    start = (fun os -> prev := default; set_style default os);
    set = (fun os style -> 
      if style <> !prev then begin 
        prev := style; close_style os; set_style style os 
      end); 
    eol = (fun _ -> ());
    finish = close_style;
  }

let term_styler = 
  let open Gfx.Style in
  let get_colour = function
    | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
    | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
  in
  let prev = ref None in
  let set_style style os = 
    os (Printf.sprintf "\027[%i;%i%sm" 
      (get_colour style.bg + 40) (get_colour style.fg + 30) (if style.bold then ";1" else ""))
  in
  let close_style os = os "\027[0m" in
  { 
    start = (fun os -> prev := None);
    set = 
        (fun os style -> 
          let set_style () = prev := Some style; set_style style os in
          match !prev with
          | Some(prev') when style <> prev' -> set_style ()
          | None -> set_style ()
          | _ -> ()); 
    eol = (fun os -> prev := None; close_style os);
    finish = close_style;
  }

let write_html_escape ?styler os ctx = 
  let open Gfx in
  let open In_memory in
  let bounds = Api.get_bounds ctx in
  let () = match styler with Some(s) -> s.start os | None -> () in 
  for r=0 to bounds.h-1 do
    for c=0 to bounds.w-1 do (* TODO styling *)
      let () = match styler with Some(s) -> s.set os (snd ctx.(r).(c)) | None -> () in 
      os ("&#" ^ string_of_int (fst ctx.(r).(c)))
    done;
    let () = match styler with Some(s) -> s.eol os | None -> () in 
    os "\n"
  done;
  let () = match styler with Some(s) -> s.finish os | None -> () in 
  ()

let write_utf8 ?styler os ctx = 
  let open Gfx in
  let open In_memory in
  let put c = 
    if c <= 0x7f then begin os (String.init 1 (fun _ -> Char.chr c))
    end else if c <= 0x7FF then begin
      os (String.init 2 (function
        | 0 -> Char.chr ((c lsr 6) lor 0b11000000)
        | _ -> Char.chr ((c land 0b00111111) lor 0b10000000)))
    end else if c <= 0xFFFF then begin
      os (String.init 3 (function
        | 0 -> Char.chr ((c lsr 12) lor 0b11100000)
        | 1 -> Char.chr (((c lsr 6) land 0b00111111) lor 0b10000000)
        | _ -> Char.chr ((c land 0b00111111) lor 0b10000000)))
    end else
      failwith "extend utf-8 writer!"
  in
  let bounds = Api.get_bounds ctx in
  let () = match styler with Some(s) -> s.start os | None -> () in 
  for r=0 to bounds.h-1 do
    for c=0 to bounds.w-1 do 
      let () = match styler with Some(s) -> s.set os (snd ctx.(r).(c)) | None -> () in 
      put (fst ctx.(r).(c))
    done;
    let () = match styler with Some(s) -> s.eol os | None -> () in 
    os "\n"
  done;
  let () = match styler with Some(s) -> s.finish os | None -> () in 
  ()

