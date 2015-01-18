open Gfx
open Data

module R = Data.Make(Gfx.In_memory.Api)

let main () = 
  let open R in
  let () = parse_args () in
  let ctx = Gfx.In_memory.init ~rows:!rows ~cols:!cols in
  let state = get_state !cols !width !height in
  draw ctx state;

  print_string 
    ("<html><head><meta charset=\"UTF-8\"><style>" ^ 
    Write.css_classes ^ 
    "</style></head><body><pre>\n");
  Write.html_escape ~styler:!styler print_string ctx;
  print_string "</pre></body>"

let () = main()

