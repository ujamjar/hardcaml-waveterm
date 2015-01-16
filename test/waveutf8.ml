open Gfx
open Data

module R = Data.Make(Gfx.In_memory.Api)

let main () = 
  let open R in
  styler := Write.term_styler;
  let () = parse_args () in
  let ctx = Gfx.In_memory.init ~rows:!rows ~cols:!cols in
  let state = get_state !cols !width !height in
  draw ctx state;

  Write.utf8 ~styler:!styler print_string ctx

let () = main()

