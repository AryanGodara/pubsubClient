open Runtime_events
open Cmdliner

let midi_message_data note volume channel =
  let status = 0b1001_0000 lor channel in
  let data1 = int_of_char (note) in
  let data2 = int_of_char (volume) in
  (status, data1, data2)

let starting_time = ref None

let adjust_time ts =
  (* The ints64 representing the duration of a runtime phase are in units of nanoseconds, whereas the ints32 representing the timestamps of midi notes are in units of miliseconds.
     So this function indirectly  multiplies the timestamp by a a factor 1000 (intentionally). *)
  let int64_to_32 i = Int32.of_int @@ Int64.to_int @@ i in
  Option.map
    (fun st ->
      Int64.sub (Timestamp.to_int64 ts) (Timestamp.to_int64 st) |> int64_to_32)
    !starting_time

let runtime_counter device tones _domain_id ts counter _value =
  match counter with
  | EV_C_MINOR_PROMOTED ->
      starting_time := Some ts;
      Midi.(
        let { Scale.note; volume } = tones 0 in
        let _status, data1, data2 = midi_message_data note volume 0 in
        let midi_message = Rtpmidi.MIDI_MESSAGE.create ~message_type:Rtpmidi.NOTE_ON ~channel:1 ~data1 ~data2 ~timestamp:0
        in
        let bytes = Rtpmidi.UDP_SERIALIZER.serialize midi_message
        in
        ( UdpClient.create_client (String.of_bytes bytes)) |> ignore);
        ()
  | _ -> ()

let runtime_begin device tones _domain_id ts event =
  let { Midi.Scale.note; volume } = Play.event_to_note tones event in
  match adjust_time ts with
  | None -> ()
  | Some ts ->
      Midi.(
        let _status, data1, data2 = midi_message_data note volume 0 in
        let midi_message = Rtpmidi.MIDI_MESSAGE.create ~message_type:Rtpmidi.NOTE_ON ~channel:1 ~data1 ~data2 ~timestamp:(Int32.to_int ts)
        in
        let bytes = Rtpmidi.UDP_SERIALIZER.serialize midi_message
        in
        ( UdpClient.create_client (String.of_bytes bytes)) |> ignore);
        ()

let runtime_end device tones _domain_id ts event =
  let { Midi.Scale.note; volume } = Play.event_to_note tones event in
  match adjust_time ts with
  | None -> ()
  | Some ts ->
      Midi.(
        let _status, data1, data2 = midi_message_data note volume 0 in
        let midi_message = Rtpmidi.MIDI_MESSAGE.create ~message_type:Rtpmidi.NOTE_ON ~channel:1 ~data1 ~data2 ~timestamp:(Int32.to_int ts)
        in
        let bytes = Rtpmidi.UDP_SERIALIZER.serialize midi_message
        in
        ( UdpClient.create_client (String.of_bytes bytes)) |> ignore);
        ()

let tracing device child_alive path_pid tones =
  let c = create_cursor path_pid in
  let runtime_begin = runtime_begin device tones in
  let runtime_end = runtime_end device tones in
  let runtime_counter = runtime_counter device tones in
  let cbs = Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter () in
  let watchdog_domain = Domain.spawn (Watchdog.watchdog_func child_alive) in
  while not (Atomic.get Watchdog.terminate) do
    ignore (read_poll c cbs None);
    Unix.sleepf 0.1
  done;
  Domain.join watchdog_domain

let simple_play = Play.play ~tracing
let play_t = Term.(const simple_play $ Play.device_id $ Play.scale $ Play.argv)
let cmd = Cmd.v (Cmd.info "simple_engine") play_t
