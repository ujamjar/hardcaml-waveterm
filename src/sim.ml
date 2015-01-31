(* connect to simulator *)
open HardCaml

module Make(B : Comb.S)(W : Wave.W with type elt = B.t) = struct

  open Cyclesim.Api

  let wrap ?cfg sim = 
    
    let cycle = ref 0 in

    let compare =
      match cfg with
      | None -> compare
      | Some(s) -> 
          let f = 
            let s = List.mapi (fun i (n,_) -> n,i) s in
            (fun x -> try Some(List.assoc x s) with _ -> None)
          in
          (fun a b ->
            match f a, f b with
            | None, None -> compare a b
            | Some _, None -> -1
            | None, Some _ -> 1
            | Some(a), Some(b) -> compare a b)
    in

    let get_type = function
      | None -> (fun _ -> W.B)
      | Some(l) -> (fun n -> try List.assoc n l with _ -> W.B)
    in

    let port (n,v) = 
        match n with
        | "clock" | "clk" -> W.Clock n, (fun _ -> ())
        | "reset" | "rst" -> 
          let d = W.make() in 
          W.Binary(n, d), (fun v -> W.set d !cycle (if v then B.vdd else B.gnd))
        | _ -> 
          let t = get_type cfg n in
          let d = W.make () in
          let wave = 
            if B.width !v = 1 && t = W.B then W.Binary(n, d)
            else W.Data(n, d, t)
          in
          wave, (fun _ -> W.set d !cycle !v)
    in

    let ports = List.concat [
      List.map port (in_ports sim);
      List.map port (out_ports sim);
      List.map port (internal_ports sim); ]
    in
    
    let ports = List.sort (fun (w0,_) (w1,_) -> compare (W.get_name w0) (W.get_name w1)) ports in

    let waves = Array.of_list (List.map fst ports) in
    let updates = Array.of_list (List.map snd ports) in

    let tasks sim rst = fun () ->
      sim ();
      Array.iter (fun f -> f rst) updates; 
      incr cycle 
    in

    { sim with
      sim_reset = tasks sim.sim_reset true;
      sim_cycle_seq = tasks sim.sim_cycle_seq false;
    }, waves

end

