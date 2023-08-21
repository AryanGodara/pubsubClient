val event_to_note : (int -> 'a) -> Runtime_events.runtime_phase -> 'a
val handle_control_c : unit -> Sys.signal_behavior
val play :
  tracing:(Midi.Device.t ->
           (unit -> bool) ->
           (string * int) option -> (int -> Midi.Scale.note_data) -> unit) ->
  int -> Midi.Scale.t -> string list -> int
val argv : string list Cmdliner.Term.t
val device_id : int Cmdliner.Term.t
val scale_enum : Midi.Scale.t Cmdliner.Arg.conv
val scale : Midi.Scale.t Cmdliner.Term.t
