open OUnit

(* Serialise a packet to a string; read it back from the string; check that the
 * resulting packet equals the one we started with. *)
let test_message_encode_decode packet =
  let data = Osc_string.of_packet packet in
  match Osc_string.to_packet data with
  | `Ok received_packet ->
    Test_common.assert_packets_equal
      packet
      received_packet
  | `Error `Missing_typetag_string ->
    failwith "Missing typetag string"
  | `Error (`Unsupported_typetag tag) ->
    failwith (Printf.sprintf "Unsupported typetag: %c" tag)

let suite =
  "string_suite" >::: (
    List.map
      (fun (name, packet) ->
        name >:: (fun () -> test_message_encode_decode packet))
      Test_common.test_packets
  )

let _ =
  print_endline "-------- String tests --------";
  Test_common.run suite
