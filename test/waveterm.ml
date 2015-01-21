open Lwt
open LTerm_geom
open LTerm_key
open CamomileLibrary

module B = HardCaml.Bits.Comb.IntbitsList
module W = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Ui = HardCamlWaveLTerm.Ui.Make(B)(W)

let rand length bits = 
  let open Ui in
  let w = W.make () in
  for i=0 to length-1 do
    W.set w i (B.srand bits)
  done;
  w

let get_waves wave_width wave_height = 
  let open Ui in
  W.{
    wave_width;
    wave_height;
    wave_cycle = 0;
    waves  = [|
      W.Clock "clock";
      W.Binary("a", rand 50 1);
      W.Data("b", rand 50 10, B.to_bstr);
      W.Data("c", rand 50 4, (fun s -> Printf.sprintf "%1x" (B.to_int s)));
      W.Data("data_out_port", rand 50 6, (fun s -> Printf.sprintf "%i" (B.to_sint s)));
    |];
  }

let run () =
  let waves = get_waves 3 1 in
  Ui.run waves
  
lwt () = run ()

