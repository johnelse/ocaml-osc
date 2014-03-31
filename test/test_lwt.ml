open OUnit

let test_udp_send_recv () =
  let open Lwt in
  let open Osc_lwt.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 4567 in
  let addr = Lwt_unix.ADDR_INET (localhost, port) in
  let buffer_length = 1024 in
  bracket
    (fun () ->
      Lwt_main.run
        (Client.create ()
        >>= (fun client -> Server.create addr buffer_length
        >>= (fun server -> return (client, server)))))
    (fun (client, server) ->
      let mvar = Lwt_mvar.create_empty () in
      Lwt_main.run
        (Lwt.async (fun () ->
          Server.recv server
          >>= (fun (packet, _) -> Lwt_mvar.put mvar packet));
        Client.send client addr Test_common.test_message_packet
        >>= (fun () -> Lwt_mvar.take mvar
        >>= (fun received_message_packet ->
          Test_common.assert_packets_equal
            Test_common.test_message_packet
            received_message_packet;
          return ()))))
    (fun (client, server) ->
      Lwt_main.run
        (Client.destroy client
        >>= (fun () -> Server.destroy server)))
    ()

let suite =
  "lwt_suite" >:::
    [
      "test_udp_send_recv" >:: test_udp_send_recv;
    ]

let _ =
  print_endline "-------- Lwt tests --------";
  run_test_tt_main suite
