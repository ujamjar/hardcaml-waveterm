(* auto generated *)
module LTerm_scroll_impl :
  sig
    class t : string -> LTerm_widget.t
    val hbar : int
    val vbar : int
    val map_range : int -> int -> int -> int
    class adjustment :
      object
        val mutable offset : int
        val offset_change_callbacks : (int -> unit) Lwt_sequence.t
        val mutable range : int
        method offset : int
        method on_offset_change :
          ?switch:LTerm_widget_callbacks.switch -> (int -> unit) -> unit
        method range : int
        method set_offset : ?trigger_callback:bool -> int -> unit
        method set_range : ?trigger_callback:bool -> int -> unit
      end
    class scrollable_adjustment :
      object
        val mutable document_size : int
        val mutable max_scroll_bar_size : int option
        val mutable min_scroll_bar_size : int option
        val mutable mouse_mode : [ `auto | `middle | `ratio ]
        val mutable offset : int
        val offset_change_callbacks : (int -> unit) Lwt_sequence.t
        val mutable page_size : int
        val mutable range : int
        val mutable scroll_bar_mode : [ `dynamic of int | `fixed of int ]
        val mutable scroll_bar_offset : int
        val mutable scroll_bar_size : int
        val mutable scroll_window_size : int
        val scrollbar_change_callbacks : (unit -> unit) Lwt_sequence.t
        method calculate_range : int -> int -> int
        method decr : int
        method document_size : int
        method get_render_params : int * int * int
        method incr : int
        method private max_scroll_bar_size : int
        method private min_scroll_bar_size : int
        method private mouse_scale_auto : int -> int
        method private mouse_scale_middle : int -> int
        method private mouse_scale_ratio : int -> int
        method mouse_scroll : int -> int
        method offset : int
        method on_offset_change :
          ?switch:LTerm_widget_callbacks.switch -> (int -> unit) -> unit
        method on_scrollbar_change :
          ?switch:LTerm_widget_callbacks.switch -> (unit -> unit) -> unit
        method page_next : int
        method page_prev : int
        method page_size : int
        method range : int
        method private scroll_bar_size : int
        method private scroll_bar_size_dynamic : int -> int
        method private scroll_bar_size_fixed : int -> int
        method private scroll_bar_steps : int
        method private scroll_of_mouse : int -> int
        method private scroll_of_window : int -> int
        method private scroll_window_size : int
        method set_document_size : int -> unit
        method set_max_scroll_bar_size : int -> unit
        method set_min_scroll_bar_size : int -> unit
        method set_mouse_mode : [ `auto | `middle | `ratio ] -> unit
        method set_offset : ?trigger_callback:bool -> int -> unit
        method set_page_size : int -> unit
        method set_range : ?trigger_callback:bool -> int -> unit
        method set_scroll_bar_mode :
          [ `dynamic of int | `fixed of int ] -> unit
        method private set_scroll_bar_offset : int -> unit
        method set_scroll_window_size : int -> unit
        method private update_page_and_document_sizes : int -> int -> unit
        method private window_of_scroll : int -> int
      end
    class virtual scrollbar :
      string ->
      bool ->
      #scrollable_adjustment ->
      object
        val mutable bar_style : [ `filled | `outline ]
        val mutable focused_style : LTerm_style.t
        val mutable show_track : bool
        val mutable unfocused_style : LTerm_style.t
        method allocation : LTerm_geom.rect
        method can_focus : bool
        method children : LTerm_widget.t list
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method private draw_bar :
          LTerm_draw.context -> LTerm_style.t -> LTerm_geom.rect -> unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method mouse_event : LTerm_event.t -> bool
        method private virtual mouse_offset :
          LTerm_mouse.t -> LTerm_geom.rect -> int
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method resource_class : string
        method resources : LTerm_resources.t
        method private virtual scroll_decr_key : LTerm_key.t
        method private virtual scroll_incr_key : LTerm_key.t
        method scroll_key_event : LTerm_event.t -> bool
        method send_event : LTerm_event.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
    class vscrollbar :
      ?rc:string ->
      ?default_event_handler:bool ->
      ?width:int ->
      #scrollable_adjustment ->
      object
        val mutable bar_style : [ `filled | `outline ]
        val mutable focused_style : LTerm_style.t
        val scroll_decr_key : LTerm_key.t
        val scroll_incr_key : LTerm_key.t
        val mutable show_track : bool
        val mutable unfocused_style : LTerm_style.t
        method allocation : LTerm_geom.rect
        method can_focus : bool
        method children : LTerm_widget.t list
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method private draw_bar :
          LTerm_draw.context -> LTerm_style.t -> LTerm_geom.rect -> unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method mouse_event : LTerm_event.t -> bool
        method private mouse_offset : LTerm_mouse.t -> LTerm_geom.rect -> int
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method resource_class : string
        method resources : LTerm_resources.t
        method private scroll_decr_key : LTerm_key.t
        method private scroll_incr_key : LTerm_key.t
        method scroll_key_event : LTerm_event.t -> bool
        method send_event : LTerm_event.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
    class hscrollbar :
      ?rc:string ->
      ?default_event_handler:bool ->
      ?height:int ->
      #scrollable_adjustment ->
      object
        val mutable bar_style : [ `filled | `outline ]
        val mutable focused_style : LTerm_style.t
        val scroll_decr_key : LTerm_key.t
        val scroll_incr_key : LTerm_key.t
        val mutable show_track : bool
        val mutable unfocused_style : LTerm_style.t
        method allocation : LTerm_geom.rect
        method can_focus : bool
        method children : LTerm_widget.t list
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method private draw_bar :
          LTerm_draw.context -> LTerm_style.t -> LTerm_geom.rect -> unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method mouse_event : LTerm_event.t -> bool
        method private mouse_offset : LTerm_mouse.t -> LTerm_geom.rect -> int
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method resource_class : string
        method resources : LTerm_resources.t
        method private scroll_decr_key : LTerm_key.t
        method private scroll_incr_key : LTerm_key.t
        method scroll_key_event : LTerm_event.t -> bool
        method send_event : LTerm_event.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
    class vslider :
      int ->
      object
        val mutable bar_style : [ `filled | `outline ]
        val mutable focused_style : LTerm_style.t
        val scroll_decr_key : LTerm_key.t
        val scroll_incr_key : LTerm_key.t
        val mutable show_track : bool
        val mutable unfocused_style : LTerm_style.t
        method allocation : LTerm_geom.rect
        method can_focus : bool
        method children : LTerm_widget.t list
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method private draw_bar :
          LTerm_draw.context -> LTerm_style.t -> LTerm_geom.rect -> unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method mouse_event : LTerm_event.t -> bool
        method private mouse_offset : LTerm_mouse.t -> LTerm_geom.rect -> int
        method offset : int
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method on_offset_change :
          ?switch:LTerm_widget_callbacks.switch -> (int -> unit) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method range : int
        method resource_class : string
        method resources : LTerm_resources.t
        method private scroll_decr_key : LTerm_key.t
        method private scroll_incr_key : LTerm_key.t
        method scroll_key_event : LTerm_event.t -> bool
        method send_event : LTerm_event.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_offset : ?trigger_callback:bool -> int -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_range : ?trigger_callback:bool -> int -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
    class hslider :
      int ->
      object
        val mutable bar_style : [ `filled | `outline ]
        val mutable focused_style : LTerm_style.t
        val scroll_decr_key : LTerm_key.t
        val scroll_incr_key : LTerm_key.t
        val mutable show_track : bool
        val mutable unfocused_style : LTerm_style.t
        method allocation : LTerm_geom.rect
        method can_focus : bool
        method children : LTerm_widget.t list
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method private draw_bar :
          LTerm_draw.context -> LTerm_style.t -> LTerm_geom.rect -> unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method mouse_event : LTerm_event.t -> bool
        method private mouse_offset : LTerm_mouse.t -> LTerm_geom.rect -> int
        method offset : int
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method on_offset_change :
          ?switch:LTerm_widget_callbacks.switch -> (int -> unit) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method range : int
        method resource_class : string
        method resources : LTerm_resources.t
        method private scroll_decr_key : LTerm_key.t
        method private scroll_incr_key : LTerm_key.t
        method scroll_key_event : LTerm_event.t -> bool
        method send_event : LTerm_event.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_offset : ?trigger_callback:bool -> int -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_range : ?trigger_callback:bool -> int -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
  end
class adjustment : LTerm_scroll_impl.adjustment
class type scrollable_adjustment =
  object
    val mutable offset : int
    val offset_change_callbacks : (int -> unit) Lwt_sequence.t
    val mutable range : int
    method decr : int
    method incr : int
    method mouse_scroll : int -> int
    method offset : int
    method on_offset_change :
      ?switch:LTerm_widget_callbacks.switch -> (int -> unit) -> unit
    method on_scrollbar_change :
      ?switch:LTerm_widget_callbacks.switch -> (unit -> unit) -> unit
    method range : int
    method set_max_scroll_bar_size : int -> unit
    method set_min_scroll_bar_size : int -> unit
    method set_mouse_mode : [ `auto | `middle | `ratio ] -> unit
    method set_offset : ?trigger_callback:bool -> int -> unit
    method set_range : ?trigger_callback:bool -> int -> unit
    method set_scroll_bar_mode : [ `dynamic of int | `fixed of int ] -> unit
  end
class type scrollable_document =
  object
    method calculate_range : int -> int -> int
    method document_size : int
    method page_next : int
    method page_prev : int
    method page_size : int
    method set_document_size : int -> unit
    method set_page_size : int -> unit
  end
class type scrollable_private =
  object
    method get_render_params : int * int * int
    method set_scroll_window_size : int -> unit
  end
class type default_scroll_events =
  object
    method mouse_event : LTerm_event.t -> bool
    method scroll_key_event : LTerm_event.t -> bool
  end
class scrollable : LTerm_scroll_impl.scrollable_adjustment
class vscrollbar :
  ?rc:string ->
  ?default_event_handler:bool ->
  ?width:int ->
  #LTerm_scroll_impl.scrollable_adjustment -> LTerm_scroll_impl.vscrollbar
class hscrollbar :
  ?rc:string ->
  ?default_event_handler:bool ->
  ?height:int ->
  #LTerm_scroll_impl.scrollable_adjustment -> LTerm_scroll_impl.hscrollbar
class vslider : int -> LTerm_scroll_impl.vslider
class hslider : int -> LTerm_scroll_impl.hslider
module Button :
  sig
    class button :
      ?brackets:string * string ->
      Zed_utf8.t ->
      object
        val click_callbacks : (unit -> unit) Lwt_sequence.t
        val mutable focused_style : LTerm_style.t
        val mutable label : Zed_utf8.t
        val mutable size_request : LTerm_geom.size
        val mutable unfocused_style : LTerm_style.t
        method allocation : LTerm_geom.rect
        method private apply_style :
          LTerm_draw.context -> LTerm_widget.t -> unit
        method can_focus : bool
        method children : LTerm_widget.t list
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method label : Zed_utf8.t
        method on_click :
          ?switch:LTerm_widget_callbacks.switch -> (unit -> unit) -> unit
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method resource_class : string
        method resources : LTerm_resources.t
        method send_event : LTerm_event.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_label : Zed_utf8.t -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
  end
module Frame :
  sig
    val draw_frame_labelled :
      LTerm_draw.context ->
      LTerm_geom.rect ->
      ?style:LTerm_style.t ->
      ?alignment:LTerm_geom.horz_alignment ->
      string -> LTerm_draw.connection -> unit
    class frame :
      object
        val mutable align : LTerm_geom.horz_alignment
        val mutable child : LTerm_widget.t option
        val mutable connection : LTerm_draw.connection
        val mutable label : string
        val mutable size_request : LTerm_geom.size
        val mutable style : LTerm_style.t
        method allocation : LTerm_geom.rect
        method can_focus : bool
        method children : LTerm_widget.t list
        method private compute_allocation : unit
        method private compute_size_request : unit
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method empty : unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method resource_class : string
        method resources : LTerm_resources.t
        method send_event : LTerm_event.t -> unit
        method set : #LTerm_widget.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_label :
          ?alignment:LTerm_geom.horz_alignment -> string -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
  end
module Spacing :
  sig
    class spacing :
      ?rows:int ->
      ?cols:int ->
      unit ->
      object
        val size_request : LTerm_geom.size
        method allocation : LTerm_geom.rect
        method can_focus : bool
        method children : LTerm_widget.t list
        method cursor_position : LTerm_geom.coord option
        method draw : LTerm_draw.context -> LTerm_widget.t -> unit
        method focus : LTerm_widget.t option LTerm_geom.directions
        method on_event :
          ?switch:LTerm_widget_callbacks.switch ->
          (LTerm_event.t -> bool) -> unit
        method parent : LTerm_widget.t option
        method queue_draw : unit
        method resource_class : string
        method resources : LTerm_resources.t
        method send_event : LTerm_event.t -> unit
        method set_allocation : LTerm_geom.rect -> unit
        method set_focus :
          LTerm_widget.t option LTerm_geom.directions -> unit
        method set_parent : LTerm_widget.t option -> unit
        method set_queue_draw : (unit -> unit) -> unit
        method set_resource_class : string -> unit
        method set_resources : LTerm_resources.t -> unit
        method size_request : LTerm_geom.size
        method update_resources : unit
      end
  end
