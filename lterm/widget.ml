
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

  let draw ~draw ~label ?style ~ctx ?border ~focused state = 
    let { rows; cols } = LTerm_draw.size ctx in
    let bounds = (*Render.Bounds.shrink_for_border*) { Gfx.r=0; c=0; w=cols; h=rows } in
    (*let border = 
      match border with
      | None -> Gfx.Style.default
      | Some(x) -> x
    in
    let border = { border with Gfx.Style.bold = focused } in*)
    (*R.with_border ~*)draw (*~label*) ?style ~ctx ~bounds (*~border*) state

  class signals cols state = object(self)
    inherit t "signals" as super
    inherit default_scrollable_document as scroll

    method can_focus = true 

    val max_signal_width = R.get_max_signal_width state 
    val max_signals = R.get_max_signals state

    method size_request = { rows=0; cols }

    method document_size = { rows=0; cols=max_signal_width }
    method page_size = size_of_rect self#allocation 

    method set_allocation r = 
      super#set_allocation r;
      scroll#set_document_size self#document_size;
      scroll#set_page_size self#page_size

    val mutable style = colour_on_black

    val mutable vscroll = 0
    method set_vscroll o = vscroll <- o

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      state.W.cfg.W.signal_scroll <- scroll#hscroll#offset;
      state.W.cfg.W.start_signal <- vscroll;
      draw ~draw:R.draw_signals ~label:"Signals" 
        ~style:style.signals ~ctx ?border:style.border ~focused state

  end

  class values cols state = object(self)
    inherit t "values" as super
    inherit default_scrollable_document as scroll

    method can_focus = true

    val max_value_width = R.get_max_value_width state (* XXX potentially slow... *)
    val max_signals = R.get_max_signals state

    method size_request = { rows=0; cols }

    method document_size = { rows=0; cols=max_value_width }
    method page_size = size_of_rect self#allocation 

    method set_allocation r = 
      super#set_allocation r;
      scroll#set_document_size self#document_size;
      scroll#set_page_size self#page_size;
      scroll#hscroll#set_offset (scroll#hscroll#range-1)

    val mutable style = colour_on_black

    val mutable vscroll = 0
    method set_vscroll o = vscroll <- o

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      state.W.cfg.W.value_scroll <- scroll#hscroll#range - scroll#hscroll#offset - 1;
      state.W.cfg.W.start_signal <- vscroll;
      draw ~draw:R.draw_values ~label:"Values" 
        ~style:style.values ~ctx ?border:style.border ~focused state

  end

  class waves state = object(self)
    inherit t "waves" as super
    inherit default_scrollable_document as scroll

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
      scroll#set_document_size self#document_size;
      scroll#set_page_size self#page_size

    initializer self#on_event @@ fun ev ->
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
      | LTerm_event.Mouse({button=Button1; control=false} as m) 
        when in_rect alloc (coord m) -> 
        pick m (fun cycle signal -> state.W.cfg.W.wave_cursor <- cycle)
      | LTerm_event.Mouse({button=Button1; control=true} as m) 
        when in_rect alloc (coord m) -> 
        pick m (fun cycle signal -> scroll#hscroll#set_offset cycle)

      | LTerm_event.Mouse {button=Button5; control} ->
          (if control then scroll#hscroll#incr else scroll#vscroll#incr); true
      | LTerm_event.Mouse {button=Button4; control} ->
          (if control then scroll#hscroll#decr else scroll#vscroll#decr); true
      | LTerm_event.Mouse {button=Button7} ->
          scroll#hscroll#incr; true
      | LTerm_event.Mouse {button=Button6} ->
          scroll#hscroll#decr; true

      (* vertical scale *)
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '+' ->
        state.W.cfg.W.wave_height <- state.W.cfg.W.wave_height + 1;
        scroll#set_page_size self#page_size;
        self#queue_draw; true
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '_' ->
        state.W.cfg.W.wave_height <- max 0 (state.W.cfg.W.wave_height - 1);
        scroll#set_page_size self#page_size;
        self#queue_draw; true

      (* Horizontal scale *)
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '-' ->
        state.W.cfg.W.wave_width <- state.W.cfg.W.wave_width - 1;
        scroll#set_page_size self#page_size;
        self#queue_draw; true
      | LTerm_event.Key{ code = Char c } when UChar.char_of c = '=' ->
        state.W.cfg.W.wave_width <- state.W.cfg.W.wave_width + 1;
        scroll#set_page_size self#page_size;
        self#queue_draw; true

      | _ -> false

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      state.W.cfg.W.start_cycle <- scroll#hscroll#offset;
      state.W.cfg.W.start_signal <- scroll#vscroll#offset;
      draw ~draw:R.draw_wave ~label:"Waves" 
        ~style:style.waves ~ctx ?border:style.border ~focused state
  
  end

  class status state = object(self)
    inherit t "status"

    method can_focus = false

    method size_request = {rows=1; cols=0}

    val mutable style = colour_on_black

    method draw ctx focused = 
      let focused = focused = (self :> t) in
      draw ~draw:R.draw_status ~label:"Status" 
        ~style:style.waves ~ctx ?border:style.border ~focused state
  
  end

  let add_scroll widget = 
    let vbox = new vbox in
    let bl, br = new button ~brackets:("","") "<", new button ~brackets:("", "") ">" in
    let hbox = new hbox in
    let hscroll = new hscrollbar ~height:1 () in
    let widget = widget hscroll in
    bl#on_click (fun () -> hscroll#set_offset (hscroll#offset-1));
    br#on_click (fun () -> hscroll#set_offset (hscroll#offset+1));
    hbox#add ~expand:false bl;
    hbox#add hscroll; 
    hbox#add ~expand:false br;
    vbox#add ~expand:true widget;
    vbox#add ~expand:false hbox;
    vbox

  let add_wscroll name widget = 
    let vbox = new vbox in
    let frame = new frame in
    frame#set_label name;
    let bl, br = new buttonx "<", new buttonx ">" in
    let hbox = new hbox in
    let hscroll = new hscrollbar_for_document ~height:1 widget in
    frame#set widget;
    bl#on_click (fun () -> widget#hscroll#set_offset (widget#hscroll#offset-1));
    br#on_click (fun () -> widget#hscroll#set_offset (widget#hscroll#offset+1));
    hbox#add ~expand:false bl;
    hbox#add hscroll; 
    hbox#add ~expand:false br;
    vbox#add ~expand:true frame;
    vbox#add ~expand:false hbox;
    vbox

  let make ?(signal_width=20) ?(value_width=20) waves = 
    
    let signal' = new signals 20 waves in
    let value' = new values 20 waves in
    let wave' = new waves waves in

    let signal = add_wscroll "Signals" signal' in
    let value = add_wscroll "Values" value' in
    let wave = add_wscroll "Waves" wave' in

    let status = new status waves in

    let vscroll = new vscrollbar_for_document ~width:1 wave' in
    wave'#vscroll#on_offset_change signal'#set_vscroll;
    wave'#vscroll#on_offset_change value'#set_vscroll;
    let bu, bd = new buttonx "^", new buttonx "v" in
    let vbox = new vbox in
    bu#on_click (fun () -> wave'#vscroll#set_offset (wave'#vscroll#offset-1));
    bd#on_click (fun () -> wave'#vscroll#set_offset (wave'#vscroll#offset+1));
    vbox#add ~expand:false bu;
    vbox#add ~expand:true vscroll;
    vbox#add ~expand:false bd;
    vbox#add ~expand:false (new spacing ~rows:1 ~cols:1 ());

    let hbox = new hbox in
    hbox#add ~expand:false signal;
    hbox#add ~expand:false value;
    hbox#add ~expand:true wave;
    hbox#add ~expand:false vbox;

    let vbox = new vbox in
    vbox#add hbox;
    let frame = new frame in
    frame#set status;
    frame#set_label "Status";
    vbox#add ~expand:false frame;

    let debug_label = new label "foo" in
    ignore (Lwt_engine.on_timer 0.1 true (fun _ -> debug_label#set_text @@
      Printf.sprintf "scroll [%i/%i] window [%ix%i] doc=[%ix%i] page=[%ix%i]" 
        wave'#vscroll#offset wave'#vscroll#range
        (size_of_rect wave'#allocation).rows
        (size_of_rect wave'#allocation).cols
        wave'#document_size.rows
        wave'#document_size.cols
        wave'#page_size.rows
        wave'#page_size.cols
    ));
    vbox#add ~expand:false debug_label;

    vbox

end


