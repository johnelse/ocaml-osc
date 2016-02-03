open OUnit
open Rresult

let test_udp_send_recv packet =
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
          >>= Lwt_mvar.put mvar);
        Client.send client addr packet
        >>= (fun () -> Lwt_mvar.take mvar
        >>= (function
          | Ok (received_packet, _) ->
            Test_common.assert_packets_equal
              packet
              received_packet;
            return ()
          | Error `Missing_typetag_string ->
            Lwt.fail (Failure "Missing typetag string")
          | Error (`Unsupported_typetag tag) ->
            Lwt.fail (Failure (Printf.sprintf "Unsupported typetag: %c" tag))))))
    (fun (client, server) ->
      Lwt_main.run
        (Client.destroy client
        >>= (fun () -> Server.destroy server)))
    ()

let udp_send_recv =
  "udp_send_recv" >::: (
    List.map
      (fun (name, packet) ->
        name >:: (fun () -> test_udp_send_recv packet))
      Test_common.test_packets
  )

let suite =
  "lwt_suite" >:::
    [
      udp_send_recv;
    ]

let () =
  print_endline "-------- Lwt tests --------";
  Test_common.run suite
