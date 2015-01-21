(* connect to simulator *)
open HardCaml

module Make(B : Comb.S)(W : Wave.W with type elt = B.t) = struct

  open Cyclesim.Api

  let wrap sim = 
    
    let cycle = ref 0 in

    let clock = W.Clock "clock", (fun _ -> ()) in
    let reset = 
      let d = W.make () in
      W.Binary("reset", d), (fun v -> W.set d !cycle (if v then B.vdd else B.gnd))
    in

    let port (n,v) =
      let d = W.make () in
      let wave = 
        if B.width !v = 1 then W.Binary(n, d)
        else W.Data(n, d, W.B)
      in
      wave, (fun _ -> W.set d !cycle !v)
    in

    let ports = List.concat [
      [ clock; reset ];
      List.map port (in_ports sim);
      List.map port (out_ports sim);
      List.map port (internal_ports sim); ]
    in

    let waves = Array.of_list (List.map fst ports) in
    let updates = Array.of_list (List.map snd ports) in

    let waves = W.({
      wave_width = 3;
      wave_height = 1;
      wave_cycle = 0;
      waves = waves;
    }) in

    let tasks sim rst = List.concat [
      sim;
      [ fun () -> Array.iter (fun f -> f rst) updates; incr cycle ];
    ] in

    { sim with
      sim_reset = tasks sim.sim_reset true;
      sim_cycle = tasks sim.sim_cycle false;
    }, waves

end

