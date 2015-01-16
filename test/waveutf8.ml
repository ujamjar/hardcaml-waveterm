open Gfx
open Data

module Ui = Data.Ui(Gfx.In_memory.Api)

let main () = 
  styler := Write.term_styler;
  let () = parse_args () in
  let ctx = Gfx.In_memory.init ~rows:!rows ~cols:!cols in
  let state = state !cols !width !height in
  Ui.draw ctx state;

  Write.utf8 ~styler:!styler print_string ctx

let () = main()

