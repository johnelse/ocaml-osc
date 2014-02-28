open OUnit

(* Start a UDP server listening on localhost; send a packet to localhost and
 * check that the server receives the same packet. *)
let test_udp_send_recv () =
  let open Osc_unix.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 4567 in
  let addr = Unix.ADDR_INET (localhost, port) in
  let buffer_length = 1024 in
  bracket
    (fun () ->
      let client = Client.create () in
      let server = Server.create addr buffer_length in
      client, server)
    (fun (client, server) ->
      let server_receive_thread channel =
        let packet, _ = Server.recv server in
        Event.sync (Event.send channel packet)
      in
      let channel = Event.new_channel () in
      let (_: Thread.t) = Thread.create server_receive_thread channel in
      Client.send client addr Test_common.test_message_packet;
      let received_message_packet = Event.sync (Event.receive channel) in
      Test_common.assert_packets_equal
        Test_common.test_message_packet
        received_message_packet)
    (fun (client, server) ->
      Client.destroy client;
      Server.destroy server)
    ()

let suite =
  "unix_suite" >:::
    [
      "test_udp_send_recv" >:: test_udp_send_recv;
    ]
