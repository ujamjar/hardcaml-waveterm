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
    "-scheme", Arg.Symbol(["white"; "black"; "colour"],
      (function
        | "white" -> scheme := Render.Styles.black_on_white
        | "black" -> scheme := Render.Styles.white_on_black
        | "colour" -> scheme := Render.Styles.colour_on_white
        | _ -> ())), " select colour scheme";
  ]
  (fun s -> files := s :: !files) "wave viewer"
  
let get_waves name = 
  let f = open_in name in
  let w = W.read f in
  close_in f;
  w

let run file =
  let waves = get_waves file in
  Ui.run waves
  
lwt () = 
  match !files with
  | [a] -> run a
  | _ -> Lwt_io.printf "Specify 1 file to render\n"

