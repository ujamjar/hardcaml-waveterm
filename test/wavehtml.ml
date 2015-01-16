open Gfx
open Data

module Ui = Data.Ui(Gfx.In_memory.Api)

let main () = 
  let () = parse_args () in
  let ctx = Gfx.In_memory.init ~rows:!rows ~cols:!cols in
  let state = state !cols !width !height in
  Ui.draw ctx state;

  print_string "<html><head><meta charset=\"UTF-8\"></head><body><pre>\n";
  Write.html_escape ~styler:!styler print_string ctx;
  print_string "</pre></body>"

let () = main()

