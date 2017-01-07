let help = 
"Wave viewer
===========

View HardCaml digital waveform files.

$ " ^ Sys.argv.(0) ^ " [file...]

Controls
--------

ESC                   Quit
+/-                   Scale wave width
<shift> +/-           Scale wave height

<mouse wheel>         Scroll up/down
<ctrl>+<mouse wheel>  Scroll left/right
<mouse button>        Set cursor
<ctrl+mouse button>   Scroll to wave
"
open Lwt
open LTerm_geom
open LTerm_key
open CamomileLibrary

open HardCamlWaveTerm

module B = HardCaml.Bits.Comb.IntbitsList
module W = Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Widget = HardCamlWaveTerm.Widget.Make(B)(W)

let num_files = Array.length Sys.argv - 1

let () = if num_files < 1 then begin
  Printf.eprintf "No files specified";
  exit (-1)
end

let get_waves name = 
  let f = open_in name in
  let w = W.read f in
  close_in f;
  w

let file_index = ref 0
let get_file idx = Sys.argv.(1 + idx)
let next () = (!file_index+1) mod num_files
let prev () = (!file_index+num_files-1) mod num_files

let run () =
  let waiter, wakener = wait () in
  let waves = get_waves (get_file !file_index) in
  let waveform = new Widget.waveform () in
  waveform#set_waves waves;

  let buttons = 
    let hbox = new LTerm_widget.hbox in
    let left = new LTerm_waveterm_compat.Button.button ~brackets:("<-- ","") (get_file (prev ())) in
    let right = new LTerm_waveterm_compat.Button.button ~brackets:(""," -->") (get_file (next ())) in
    let exit = new LTerm_widget.button "exit" in
    let click f = 
      file_index := f ();
      waveform#set_waves ~keep_cfg:true (get_waves (get_file !file_index));
      left#set_label (get_file (prev ()));
      right#set_label (get_file (next ()))
    in
    left#on_click (fun () -> click prev);
    right#on_click (fun () -> click next);
    exit#on_click (wakeup wakener);
    hbox#add left;
    hbox#add ~expand:false exit;
    hbox#add right;
    hbox
  in

  let top = new LTerm_widget.vbox in
  top#add waveform;
  top#add ~expand:false (new LTerm_widget.hline);
  top#add ~expand:false buttons;

  top#on_event (function 
    LTerm_event.Key{LTerm_key.code=LTerm_key.Escape} -> 
      wakeup wakener (); false | _ -> false);

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> LTerm_widget.run term top waiter)
    (fun () -> LTerm.disable_mouse term)
  
let%lwt () = run ()

