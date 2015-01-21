let help = 
"Wave viewer
===========

View HardCaml digital waveform file.

$ " ^ Sys.argv.(0) ^ " [options] file

Controls
--------

q/esc             Quit
+/-               Scale wave width
<shift> +/-       Scale wave height
<arrows>          Scroll
<shift> <arrows>  Scroll x 10
"
open Lwt
open LTerm_geom
open LTerm_key
open CamomileLibrary

open HardCamlWaveTerm

module B = HardCaml.Bits.Comb.IntbitsList
module W = Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Ui = HardCamlWaveLTerm.Ui.Make(B)(W)

let scheme = ref Render.Styles.colour_on_black
let files = ref []

let () = Arg.parse
  [
    "-style", Arg.Symbol(["white"; "black"; "light"; "dark"],
      (function
        | "white" -> scheme := Render.Styles.black_on_white
        | "black" -> scheme := Render.Styles.white_on_black
        | "light" -> scheme := Render.Styles.colour_on_white
        | "dark" -> scheme := Render.Styles.colour_on_black
        | _ -> ())), " select colour scheme";
  ]
  (fun s -> files := s :: !files) help
  
let get_waves name = 
  let f = open_in name in
  let w = W.read f in
  close_in f;
  w

let run file =
  let waves = get_waves file in
  Ui.run ~style:!scheme waves
  
lwt () = 
  match !files with
  | [a] -> run a
  | _ -> Lwt_io.printf "Specify 1 file to render\n"

