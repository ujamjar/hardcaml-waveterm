(* Demonstrate a standard HardCaml testbench with a generated waveform
   written to the terminal.

   We use 'HardCamlWaveTerm.Sim.Make(B).wrap' to hook into the simulation 
   and generate the waveform data.
   
   When the sim has finished the Write.uft8 function prints the wave to stdout.
   We also display the interactive UI.

   This flow might be useful in, for example, utop or iocaml notebooks.  
   Note that you can render multiple times and at any point during the 
   simulation to capture interesting data.
*)

open HardCaml.Api
open Comb

open HardCamlWaveTerm
module W = Wave.Make(Wave.Bits(B))
module Ws = Sim.Make(B)(W)
module R = Render.Static(W)

module Ui = HardCamlWaveTerm.Ui.Make(B)(W)

module I = struct
  type 'a t = {
    a : 'a [@bits 4];
    b : 'a [@bits 4];
  }[@@deriving hardcaml]
end
module O = struct
  type 'a t = {
    c : 'a [@bits 4]; 
  }[@@deriving hardcaml]
end

open I
open O

let f i = 
  { c = i.a +: i.b }

module G = Interface.Gen(I)(O)

let circ,sim,i,o,_ = G.make "test" f
let sim, waves = Ws.wrap sim
let () =
  Cs.reset sim;
  for l=0 to 7 do
    for m=0 to 7 do
      i.a := B.consti 4 l;
      i.b := B.consti 4 m;
      Cs.cycle sim;
    done;
  done

let waves = W.({ cfg=default; waves })

(* show data in terminal *)
let () = 
  let open HardCamlWaveTerm in
  Write.(utf8 ~styler:term_styler print_string 
    (R.(draw ~style:Render.Styles.colour_on_black ~cols:200 waves)))

(* show user interface *)
let%lwt () = Ui.run waves

