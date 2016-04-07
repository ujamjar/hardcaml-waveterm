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
  let waveform = Widget.make waves in
  vbox#add waveform;
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

