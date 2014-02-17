open OUnit

let test_message_packet = Osc.(Message {
  address = "/foo/bar";
  arguments = [
    Blob "baz";
    String "quux";
    Int32 789l;
    Float32 123.456;
  ];
})

let are_arguments_equal arg1 arg2 =
  let open Osc in
  match arg1, arg2 with
  | Blob a, Blob b -> a = b
  | String a, String b -> a = b
  | Int32 a, Int32 b -> a = b
  | Float32 a, Float32 b ->
    let diff = abs_float (a -. b) in
    diff <= 0.01
  | _, _ -> false

let string_of_argument = function
  | Osc.Blob s -> Printf.sprintf "Blob %s" s
  | Osc.String s -> Printf.sprintf "String %s" s
  | Osc.Int32 i -> Printf.sprintf "Int32 %ld" i
  | Osc.Float32 f -> Printf.sprintf "Float32 %f" f

let assert_messages_equal message1 message2 =
  let open Osc in
  assert_equal
    ~msg:"Incorrect address"
    message1.address message2.address;
  assert_equal
    ~msg:"Incorrect number of arguments"
    (List.length message1.arguments)
    (List.length message2.arguments);
  List.iter2
    (fun argument1 argument2 ->
      assert_equal
        ~cmp:are_arguments_equal
        ~printer:string_of_argument
        argument1 argument2)
    message1.arguments
    message2.arguments

let assert_packets_equal packet1 packet2 =
  let open Osc in
  match packet1, packet2 with
  | Message message1, Message message2 ->
    assert_messages_equal message1 message2
  | Bundle _, Bundle _ ->
    assert_failure "Bundles not implemented"
  | _, _ ->
    assert_failure "Packet types differ"

let test_string_codec () =
  let data = Osc_string.of_packet test_message_packet in
  let received_message_packet = Osc_string.to_packet data in
  assert_packets_equal test_message_packet received_message_packet

let test_send_message () =
  bracket
    (fun () -> Unix.pipe ())
    (fun (infd, outfd) ->
      Osc_unix.Codec.(
        Encode.packet outfd test_message_packet;
        let received_message_packet = Decode.packet infd in
        assert_packets_equal test_message_packet received_message_packet
      ))
    (fun (infd, outfd) ->
      Unix.close outfd;
      Unix.close infd)
    ()

let base_suite =
  "base_suite" >:::
    [
      "test_string_codec" >:: test_string_codec;
      "test_send_message" >:: test_send_message;
    ]

let _ = run_test_tt_main base_suite
