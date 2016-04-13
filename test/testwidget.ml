open Lwt
open LTerm_widget

module B = HardCaml.Bits.Comb.IntbitsList
module W = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Widget = HardCamlWaveLTerm.Widget.Make(B)(W)

let main () =
  let waiter, wakener = wait () in

  let get_waves name = 
    let f = open_in name in
    let w = W.read f in
    close_in f;
    w
  in
  let waves = get_waves Sys.argv.(1) in

  let vbox = new vbox in
  let waveform = new Widget.waveform waves in
  vbox#add waveform;

  (* add status window *)
  let status = new Widget.status waves in
  let frame = new frame in
  frame#set status;
  frame#set_label "Status";
  vbox#add ~expand:false frame;

  (* debug *)
  let debug_label = new label "foo" in
  ignore (Lwt_engine.on_timer 0.1 true (fun _ -> debug_label#set_text @@
    Printf.sprintf "scroll [%i/%i] window [%ix%i] doc=[%ix%i] page=[%ix%i]" 
      waveform#waves#vscroll#offset waveform#waves#vscroll#range
      LTerm_geom.((size_of_rect waveform#waves#allocation).rows)
      LTerm_geom.((size_of_rect waveform#waves#allocation).cols)
      waveform#waves#document_size.LTerm_geom.rows
      waveform#waves#document_size.LTerm_geom.cols
      waveform#waves#page_size.LTerm_geom.rows
      waveform#waves#page_size.LTerm_geom.cols
  ));
  
  vbox#add ~expand:false debug_label;

  let button = new button "exit" in
  button#on_click (wakeup wakener);
  vbox#add ~expand:false button;

  vbox#on_event (function 
    LTerm_event.Key{LTerm_key.code=LTerm_key.Escape} -> 
      wakeup wakener (); false | _ -> false);

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

