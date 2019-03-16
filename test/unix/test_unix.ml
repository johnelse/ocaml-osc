open OUnit
open Result

(* Start a UDP server listening on localhost; send a packet to localhost and
 * check that the server receives the same packet. *)
let test_udp_send_recv packet =
  let open Osc_unix.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 4568 in
  let addr = Unix.ADDR_INET (localhost, port) in
  let buffer_length = 1024 in
  bracket
    (fun () ->
      let client = Client.create () in
      let server = Server.create addr buffer_length in
      client, server)
    (fun (client, server) ->
      let server_receive_thread channel =
        let result = Server.recv server in
        Event.sync (Event.send channel result)
      in
      let channel = Event.new_channel () in
      let (_: Thread.t) = Thread.create server_receive_thread channel in
      Client.send client addr packet;
      match Event.sync (Event.receive channel) with
      | Ok (received_packet, _) ->
        Test_common.assert_packets_equal
          packet
          received_packet
      | Error `Missing_typetag_string ->
        failwith "Missing typetag string"
      | Error (`Unsupported_typetag tag) ->
        failwith (Printf.sprintf "Unsupported typetag: %c" tag))
    (fun (client, server) ->
      Client.destroy client;
      Server.destroy server)
    ()

let udp_send_recv =
  "udp_send_recv" >::: (
    List.map
      (fun (name, packet) ->
        name >:: (fun () -> test_udp_send_recv packet))
      Test_common.test_packets_internal
  )

let suite =
  "unix_suite" >:::
    [
      udp_send_recv;
    ]

let () =
  print_endline "-------- Unix tests --------";
  Test_common.run suite
