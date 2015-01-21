(* Demonstrate a standard HardCaml testbench with a generated waveform
   written to the terminal.

   We use 'HardCamlWaveTerm.Sim.Make(B).wrap' to hook into the simulation 
   and generate the waveform data and the standard Write.uft8 function to 
   print it.

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

module Ui = HardCamlWaveLTerm.Ui.Make(B)(W)

module I = interface
  a[4] b[4]
end

module O = interface 
  c[4]
end

open I
open O

let f i = 
  { c = i.a +: i.b }

module G = Interface.Gen(I)(O)

let circ,sim,i,o = G.make "test" f
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

(* show data in terminal *)
let () = 
  let open HardCamlWaveTerm in
  Write.(utf8 ~styler:term_styler print_string 
    (R.(draw 
      ~style:Render.Styles.colour_on_black ~cols:200
      W.{ waves with wave_height=1; })))

(* show user interface *)
lwt () = Ui.run waves

