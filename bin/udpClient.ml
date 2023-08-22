open Lwt.Infix

(* Shared mutable counter *)
let listen_address = Unix.inet_addr_loopback
let port = 9000

let sock : Lwt_unix.file_descr Lwt.t ref = ref (Lwt.return Lwt_unix.stdin)

let handle_message msg =
  print_endline ("Received message in handle_message: " ^ msg);
  match msg with "quit" -> "quit" | _ -> "Ready for next message"

let rec handle_request server_socket midiMessage =
  server_socket >>= fun server_socket ->
  (* Wait for promise server_socket to resolve, and then use its value *)
  Logs_lwt.info (fun m ->
      m "Sending MIDI message to the UDP Server: ")
  >>= fun () ->
  Lwt_unix.sendto server_socket (Bytes.of_string midiMessage) 0
    (String.length midiMessage) []
    (ADDR_INET (listen_address, port))
  >>= fun _ ->
  print_endline "Request sent";

  let buffer = Bytes.create 1024 in
  Lwt_unix.recvfrom server_socket buffer 0 1024 [] >>= fun (num_bytes, _) ->
  print_endline "Received response from server";
  let message = Bytes.sub_string buffer 0 num_bytes in
  print_endline ("Received message in handle_request: " ^ message);
  Lwt.return_unit

let create_client message =
  print_endline "Creating client";
  handle_request !sock message

let create_socket () : Lwt_unix.file_descr Lwt.t =
  print_endline "Creating socket";
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_DGRAM 0 in
  Lwt.return sock

let init () : unit =
  sock := Lwt.return( Lwt_main.run (create_socket ()))