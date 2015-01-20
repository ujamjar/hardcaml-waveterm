(* connect to simulator *)
open HardCaml

module Make(B : Comb.S) = struct

  open Cyclesim.Api

  module D = Wave.Bits(B)
  module W = Wave.Make(D)
  module G = Gfx.In_memory.Api
  module R = Render.Make(G)(W)

  let wrap sim = 
    
    let cycle = ref 0 in

    let clock = W.Clock "clock", (fun _ -> ()) in
    let reset = 
      let d = D.make () in
      W.Binary("reset", d), (fun v -> D.set d !cycle (if v then B.vdd else B.gnd))
    in

    let port (n,v) =
      let d = D.make () in
      let wave = 
        if B.width !v = 1 then W.Binary(n, d)
        else W.Data(n, d, B.to_bstr)
      in
      wave, (fun _ -> D.set d !cycle !v)
    in

    let ports = List.concat [
      [ clock; reset ];
      List.map port (in_ports sim);
      List.map port (out_ports sim);
      List.map port (internal_ports sim); ]
    in

    let waves = Array.of_list (List.map fst ports) in
    let updates = Array.of_list (List.map snd ports) in

    let render () = 
      let open Gfx in
      let ctx = Gfx.In_memory.init ~rows:30 ~cols:79 in

      let waves = R.({
        wave_width = 3;
        wave_height = 1;
        wave_cycle = 0;
        waves = waves;
      }) in

      let style = Render.Styles.white_on_black in
      R.draw_ui ~style ~ctx waves;

      Write.utf8 ~styler:Write.no_styler print_string ctx
    in

    { sim with
      sim_reset = (fun () -> sim.sim_reset (); Array.iter (fun f -> f true) updates; incr cycle);
      sim_cycle = (fun () -> sim.sim_cycle (); Array.iter (fun f -> f false) updates; incr cycle);
    }, render

end
