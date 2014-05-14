open OUnit2

(* Serialise a packet to a string; read it back from the string; check that the
 * resulting packet equals the one we started with. *)
let test_message_encode_decode packet =
  let data = Osc_string.of_packet packet in
  let received_packet = Osc_string.to_packet data in
  Test_common.assert_packets_equal
    packet
    received_packet

let suite =
  "string_suite" >::: (
    List.map
      (fun (name, packet) ->
        name >:: (fun _ -> test_message_encode_decode packet))
      Test_common.test_packets
  )

let _ =
  print_endline "-------- String tests --------";
  run_test_tt_main suite
