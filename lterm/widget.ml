
module Make
  (B : HardCaml.Comb.S) 
  (W : HardCamlWaveTerm.Wave.W with type elt = B.t) 
= struct

  open HardCamlWaveTerm
  open Render.Styles
  open Lwt
  open LTerm_key
  open LTerm_widget
  open LTerm_geom
  open CamomileLibrary

  module G = Gfx_lterm.Api
  module R = Render.Make(G)(W)

  
  class buttonx text = object(self)
    inherit button text as button

    method size_request = 
      let b = button#size_request in
      { b with cols = b.cols-4 } (* hack; undo the extra chars around button *)

    (* has to be copied from the button class *)
    val mutable focused_style = LTerm_style.none
    val mutable unfocused_style = LTerm_style.none
    method update_resources =
      let rc = self#resource_class and resources = self#resources in
      focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
      unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources

    method private apply_style ctx focused =
      let style =
        if focused = (self :> t)
        then focused_style
        else unfocused_style
      in
      LTerm_draw.fill_style ctx style

    (* draw naked button *)
    method draw ctx focused =
      let { rows; cols } = LTerm_draw.size ctx in
      let len = Zed_utf8.length button#label in
      self#apply_style ctx focused;
      LTerm_draw.draw_string ctx (rows / 2) ((cols - len) / 2) button#label

  end

  let draw ~draw ?style ~ctx ?border ~focused state = 
    let { rows; cols } = LTerm_draw.size ctx in
    let bounds = { Gfx.r=0; c=0; w=cols; h=rows } in
    draw ?style ~ctx ~bounds state

  class waves state = object(self)
    inherit t "waves" as super

    val hscroll = new scrollable
    val vscroll = new scrollable
    method hscroll = hscroll
    method vscroll = vscroll

    method can_focus = true

    method size_request = {rows=0; cols=0}

    val mutable style = colour_on_black

    val max_cycles = R.get_max_cycles state + 1
    val max_signals = R.get_max_signals state

    method document_size = { rows=max_signals; cols=max_cycles }

    method page_size = 
      let _, cycle_width = R.get_wave_width (state.W.cfg.W.wave_width, W.Clock "") in
      let alloc = size_of_rect self#allocation in
      let page_approx_width = 
        int_of_float ((float_of_int alloc.cols /. float_of_int cycle_width) +. 0.5)
      in
      let page_approx_height = 
        let total_height = float_of_int @@ R.get_max_wave_height state 0 in
        let page_height = float_of_int alloc.rows in
        let num_signals = float_of_int max_signals in
        max 0 (min max_signals @@
          int_of_float ((page_height /. (total_height /. num_signals)) (*+. 0.5*)))
      in
      { rows = page_approx_height;
        cols = max 0 page_approx_width }

    method set_allocation r = 
      super#set_allocation r;
      hscroll#set_document_size self#document_size.cols;
      vscroll#set_document_size self#document_size.rows;
      hscroll#set_page_size self#page_size.cols;
      vscroll#set_page_size self#page_size.rows

    method private pick_event ev = 
      let open LTerm_mouse in
      let open LTerm_key in

      let alloc = self#allocation in

      let pick m f = 
        (* XXX yuk *)
        let bounds = 
          let z = Gfx.{ r=0; c=0; w=0; h=0 } in
          Render.Bounds.{
            waves = Gfx.{ r=alloc.row1; c=alloc.col1; 
                          w=alloc.col2-alloc.col1; h=alloc.row2-alloc.row1 };
            values=z; signals=z; status=z;
          } 
        in
        match R.pick ~bounds:bounds ~r:m.row ~c:m.col state with
        | R.Wave(cycle,signal) -> f cycle signal; true
        | _ -> false
      in

      match ev with

      (* cursor *)
      | LTerm_event.Mouse({button=Button1; control=false} as m) 
        when in_rect alloc (coord m) -> 
        pick m (fun cycle signal -> state.W.cfg.W.wave_cursor <- cycle)

      (* move to cycle *)
      | LTerm_event.Mouse({button=Button1; control=true} as m) 
        when in_rect alloc (coord m) -> 
        pick m (fun cycle signal -> hscroll#set_offset cycle)

      | _ -> false

    method wheel_event (hscroll : scrollable) ev = 
      let open LTerm_mouse in
      match ev with
      (* mouse wheel *)
      | LTerm_event.Mouse {button=Button5; control} ->
          (if control then hscroll#set_offset hscroll#incr 
           else vscroll#set_offset vscroll#incr); true
      | LTerm_event.Mouse {button=Button4; control} ->
          (if control then hscroll#set_offset hscroll#decr 
           else vscroll#set_offset vscroll#decr); true

      | _ -> false

    method scale_event = function
      (* vertical scale *)
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '+' ->
        state.W.cfg.W.wave_height <- state.W.cfg.W.wave_height + 1;
        let page_size = self#page_size in
        hscroll#set_page_size page_size.cols;
        vscroll#set_page_size page_size.rows;
        self#queue_draw; true
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '_' ->
        state.W.cfg.W.wave_height <- max 0 (state.W.cfg.W.wave_height - 1);
        let page_size = self#page_size in
        hscroll#set_page_size page_size.cols;
        vscroll#set_page_size page_size.rows;
        self#queue_draw; true

      (* Horizontal scale *)
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '-' ->
        state.W.cfg.W.wave_width <- state.W.cfg.W.wave_width - 1;
        let page_size = self#page_size in
        hscroll#set_page_size page_size.cols;
        vscroll#set_page_size page_size.rows;
        self#queue_draw; true
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '=' ->
        state.W.cfg.W.wave_width <- state.W.cfg.W.wave_width + 1;
        let page_size = self#page_size in
        hscroll#set_page_size page_size.cols;
        vscroll#set_page_size page_size.rows;
        self#queue_draw; true

      | _ -> false

    initializer self#on_event @@ fun ev -> 
      self#pick_event ev || 
      self#wheel_event hscroll ev || 
      self#scale_event ev
      
    initializer vscroll#add_scroll_event_handler (self#wheel_event hscroll)
    initializer hscroll#add_scroll_event_handler (self#wheel_event hscroll)

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      state.W.cfg.W.start_cycle <- hscroll#offset;
      state.W.cfg.W.start_signal <- vscroll#offset;
      draw ~draw:R.draw_wave 
        ~style:style.waves ~ctx ?border:style.border ~focused state
  
  end

  class signals cols state wave = object(self)
    inherit t "signals" as super

    val vscroll = wave#vscroll
    val hscroll = new scrollable
    method hscroll = hscroll

    method can_focus = true 

    val max_signal_width = R.get_max_signal_width state 
    val max_signals = R.get_max_signals state

    method size_request = { rows=0; cols }

    method set_allocation r = 
      super#set_allocation r;
      hscroll#set_document_size max_signal_width;
      hscroll#set_page_size (size_of_rect r).cols

    val mutable style = colour_on_black

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      state.W.cfg.W.signal_scroll <- hscroll#offset;
      state.W.cfg.W.start_signal <- vscroll#offset;
      draw ~draw:R.draw_signals 
        ~style:style.signals ~ctx ?border:style.border ~focused state

    initializer self#on_event (fun ev -> wave#wheel_event hscroll ev || wave#scale_event ev)
    initializer hscroll#add_scroll_event_handler (wave#wheel_event hscroll)

  end

  class values cols state wave = object(self)
    inherit t "values" as super

    val vscroll = wave#vscroll
    val hscroll = new scrollable
    method hscroll = hscroll

    method can_focus = true

    val mutable max_value_width = 0
    val max_signals = R.get_max_signals state

    method private set_max_value_width w = 
      if w > max_value_width then begin
        let diff = w - max_value_width in
        max_value_width <- w;
        hscroll#set_document_size max_value_width;
        hscroll#set_offset (hscroll#offset + diff);
      end

    method size_request = { rows=0; cols }

    method set_allocation r = 
      super#set_allocation r;
      hscroll#set_page_size (size_of_rect r).cols;
      self#set_max_value_width (size_of_rect r).cols;
      hscroll#set_offset 0

    val mutable style = colour_on_black

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      state.W.cfg.W.value_scroll <- hscroll#range - hscroll#offset - 1;
      state.W.cfg.W.start_signal <- vscroll#offset;
      self#set_max_value_width @@
        draw ~draw:R.draw_values 
          ~style:style.values ~ctx ?border:style.border ~focused state

    initializer self#on_event (fun ev -> wave#wheel_event hscroll ev || wave#scale_event ev)
    initializer hscroll#add_scroll_event_handler (wave#wheel_event hscroll)

  end

  class status state = object(self)
    inherit t "status"

    method can_focus = false

    method size_request = {rows=1; cols=0}

    val mutable style = colour_on_black

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      draw ~draw:R.draw_status 
        ~style:style.waves ~ctx ?border:style.border ~focused state
  
  end

  let add_scroll name widget = 
    let vbox = new vbox in
    let frame = new frame in
    frame#set_label name;
    let bl, br = new buttonx "<", new buttonx ">" in
    let hbox = new hbox in
    let hscroll = new hscrollbar ~height:1 widget#hscroll in
    frame#set widget;
    bl#on_click (fun () -> widget#hscroll#set_offset (widget#hscroll#offset-1));
    br#on_click (fun () -> widget#hscroll#set_offset (widget#hscroll#offset+1));
    hbox#add ~expand:false bl;
    hbox#add hscroll; 
    hbox#add ~expand:false br;
    vbox#add ~expand:true frame;
    vbox#add ~expand:false hbox;
    vbox

  class waveform waves = 
    let wave' = new waves waves in
    let signal' = new signals 20 waves wave' in
    let value' = new values 20 waves wave' in

    let signal = add_scroll "Signals" signal' in
    let value = add_scroll "Values" value' in
    let wave = add_scroll "Waves" wave' in

    let vscroll = new vscrollbar ~width:1 wave'#vscroll in
    let bu, bd = new buttonx "^", new buttonx "v" in
    let vbox = new vbox in
    let () = bu#on_click (fun () -> wave'#vscroll#set_offset (wave'#vscroll#offset-1)) in
    let () = bd#on_click (fun () -> wave'#vscroll#set_offset (wave'#vscroll#offset+1)) in
    let () = vbox#add ~expand:false bu in
    let () = vbox#add ~expand:true vscroll in
    let () = vbox#add ~expand:false bd in
    let () = vbox#add ~expand:false (new spacing ~rows:1 ~cols:1 ()) in

  object(self)
    inherit hbox as hbox
    initializer
      hbox#add ~expand:false signal;
      hbox#add ~expand:false value;
      hbox#add ~expand:true wave;
      hbox#add ~expand:false vbox
    
    method waves = wave'
    method signals = signal'
    method values = value'
  
  end

end


