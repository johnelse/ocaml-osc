open OUnit

(* Serialise a packet to a string; read it back from the string; check that the
 * resulting packet equals the one we started with. *)
let test_message_encode_decode () =
  let data = Osc_string.of_packet Test_common.test_message_packet in
  let received_message_packet = Osc_string.to_packet data in
  Test_common.assert_packets_equal
    Test_common.test_message_packet
    received_message_packet

let suite =
  "string_suite" >:::
    [
      "test_message_encode_decode" >:: test_message_encode_decode;
    ]

let _ = run_test_tt_main suite
