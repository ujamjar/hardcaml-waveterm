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

lwt () = 
  match_lwt Ui.run_testbench ~timeout:0.5 {waves with W.wave_width=(-1)}(testbench()) with
  | None -> Lwt_io.printf "Canceled!\n"
  | Some(x) -> Lwt_io.printf "OK!\n"

