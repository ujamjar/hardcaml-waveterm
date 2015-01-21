(* $ utop "test/genwave.ml" *)
#directory "_build"
#require "hardcaml"
#load "HardCamlWaveTerm.cma"

open HardCamlWaveTerm
open HardCaml

module B = Bits.Comb.IntbitsList
module W = Wave.Make(Wave.Bits(B))

(* generate data patterns *)
let rand length bits = W.init length (fun _ -> B.srand bits)
let toggle length n bits = W.init length 
  (fun i -> if (i/n) mod 2 = 0 then B.consti bits 0 else B.consti bits 1)
let count length bits = W.init length (B.consti bits)

let cycles = 50

let wave_toggle = 
  W.({
    wave_width = 3;
    wave_height = 1;
    wave_cycle = 0;
    waves = [|
      W.Clock "clock";
      W.Binary("b1", toggle cycles 1 1);
      W.Data  ("d1", toggle cycles 1 1, W.B);
      W.Binary("b2", toggle cycles 2 1);
      W.Data  ("d2", toggle cycles 2 1, W.B);
      W.Binary("b3", toggle cycles 3 1);
      W.Data  ("d3", toggle cycles 3 1, W.B);
      W.Binary("b4", toggle cycles 4 1);
      W.Data  ("d4", toggle cycles 4 1, W.B);
    |];
  })

let wave_data_render = 
  W.({
    wave_width = 8;
    wave_height = 4;
    wave_cycle = 10;
    waves = [|
      W.Clock "clock";
      W.Data("binary", rand cycles 2, W.B);
      W.Data("hex", rand cycles 9, W.H);
      W.Data("unsigned", rand cycles 5, W.U);
      W.Data("signed", rand cycles 5, W.S);
      W.Data("index ", rand cycles 2, W.I ["One";"Two";"Three";"Four"]);
      W.Data("function", rand cycles 4, W.F (fun e -> if B.to_int e > 90 then ">10" else ""));
    |];
  })

let write_wave n w = 
  let f = open_out n in
  W.write f w;
  close_out f

let () = write_wave "test/toggle.wave" wave_toggle
let () = write_wave "test/data-render.wave" wave_data_render

