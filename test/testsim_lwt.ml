(* This shows a LWT-ified HardCaml testbench which is run in parallel with 
   the interactive waveform viewer.

   'HardCamlWaveTerm.Sim.Make(B).wrap' hooks us into the simulation and 
   records the waveform data.

   The standard HardCaml Cs.reset and Cs.cycle calls are simply wrapped with
   Lwt.wrap1, then the testbench written in an (imperative) Lwt style.

   Lastly we use the HardCamlWaveLTerm.Ui.run_testbench function with our
   testbench thread.  In the code cycles are slowed down with a sleep call 
   so we can watch them get dynamically generated in the viewer! *)

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

(* wrap reset and cycle functions in Lwt *)
let reset sim = Lwt.wrap1 Cs.reset sim
let cycle sim = Lwt.wrap1 Cs.cycle sim >> Lwt_unix.sleep 0.1

let testbench () = 
  lwt () = reset sim in
  for_lwt l=0 to 7 do
    for_lwt m=0 to 7 do
      i.a := B.consti 4 l;
      i.b := B.consti 4 m;
      lwt () = cycle sim in
      Lwt.return ()
    done;
  done

let waves = W.{ cfg={default with wave_width=(-1)}; waves }

lwt () = 
  match_lwt Ui.run_testbench ~timeout:0.5 waves (testbench()) with
  | None -> Lwt_io.printf "Canceled!\n"
  | Some(x) -> Lwt_io.printf "OK!\n"

