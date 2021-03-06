open Lwt

open HardCaml.Api

module W = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Ws = HardCamlWaveTerm.Sim.Make(B)(W)

module Widget = HardCamlWaveTerm.Widget.Make(B)(W)
let waveform = new Widget.waveform () 

(* simple hardware design *)

let bits = 8
module I = struct
  type 'a t = {
    a : 'a [@bits bits];
    b : 'a [@bits bits];
  }[@@deriving hardcaml]
end
module O = struct
  type 'a t = {
    c : 'a [@bits bits]; 
  }[@@deriving hardcaml]
end

open I
open O

let f i = Comb.{ c = i.a +: i.b }

module G = Interface.Gen(I)(O)

(* simulation *)

let circ,sim,i,o,_ = G.make "test" f
let sim, waves = Ws.wrap sim

let reset sim = Lwt.wrap1 Cs.reset sim
let cycle sim = 
  let%lwt () = Lwt.wrap1 Cs.cycle sim in 
  Lwt_unix.sleep 0.01

let testbench () = 
  let%lwt () = reset sim in
  let%lwt () = 
    for%lwt l=0 to (1 lsl bits)-1 do
      for%lwt m=0 to (1 lsl bits)-1  do
        i.a := B.consti bits l;
        i.b := B.consti bits m;
        let%lwt () = cycle sim in
        Lwt.return ()
      done;
    done
  in
  Lwt.return "ok"

let rec update_loop () = 
  waveform#update_wave_cycles; waveform#queue_draw;
  Lwt_unix.sleep 0.25 >> update_loop ()

(* waveform *)

let main () = 
  let waves = W.{ cfg={default with wave_width=(-1)}; waves } in
  waveform#set_waves waves;
  let run = 
    let%lwt r = testbench () and () = update_loop () in
    Lwt.return r
  in
  Widget.run_widget_testbench waveform run

let () = 
  match Lwt_main.run (main ()) with
  | Some(s) -> Printf.eprintf "OK: %s\n%!" s
  | None -> Printf.eprintf "QUIT\n%!" (* why doesnt this print? *)

let () = Printf.eprintf "OVER.\n%!"

