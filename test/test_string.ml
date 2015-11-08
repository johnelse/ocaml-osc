open OUnit
open Rresult

(* Serialise a packet to a string; read it back from the string; check that the
 * resulting packet equals the one we started with. *)
let test_message_encode_decode packet =
  let data = Osc_string.of_packet packet in
  match Osc_string.to_packet data with
  | Ok received_packet ->
    Test_common.assert_packets_equal
      packet
      received_packet
  | Error `Missing_typetag_string ->
    failwith "Missing typetag string"
  | Error (`Unsupported_typetag tag) ->
    failwith (Printf.sprintf "Unsupported typetag: %c" tag)

let test_message_encode_decode_suite =
  "test_message_encode_decode" >::: (
    List.map
      (fun (name, packet) ->
        name >:: (fun () -> test_message_encode_decode packet))
      Test_common.test_packets
  )

let test_data =
  let open Osc in
  [
    (* A packet which we expect to decode successfully. *)
    "message_ok",
    "/foo/bar\000\000\000\000,is\000\000\000\000{hi\000\000",
    Ok (Message {
      address = "/foo/bar";
      arguments = [Int32 123l; String "hi"];
    });
    (* A packet which is missing a typetag string. *)
    "message_missing_typetag_string",
    "/foo/bar\000\000\000\000\000\000\000{hi\000\000",
    Error `Missing_typetag_string;
    (* A packet which contains an unsupported typetag. *)
    "message_unsupported_typetag",
    "/foo/bar\000\000\000\000,iz\000\000\000\000{hi\000\000",
    Error (`Unsupported_typetag 'z');
  ]

let test_message_decode data expected_result =
  match (Osc_string.to_packet data, expected_result) with
  | Ok decoded_packet, Ok expected_packet ->
    Test_common.assert_packets_equal
      decoded_packet expected_packet
  | result, expected_result ->
    assert_equal result expected_result

let test_message_decode_suite =
  "test_error_handling" >::: (
    List.map
      (fun (name, data, expected_result) ->
        name >:: (fun () -> test_message_decode data expected_result))
      test_data
  )

let base_suite =
  "base_suite" >:::
    [
      test_message_encode_decode_suite;
      test_message_decode_suite;
    ]

let () =
  print_endline "-------- String tests --------";
  Test_common.run base_suite
