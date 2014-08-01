open OUnit

let message_no_args = Osc.(Message {
  address = "/test";
  arguments = [];
})

let message_empty_string_arg = Osc.(Message {
  address = "/test";
  arguments = [
    String "";
  ];
})

let message_all_args_except_blobs = Osc.(Message {
  address = "/test";
  arguments = [
    String "foobar";
    Int32 123l;
    Float32 456.789;
  ];
})

let message_empty_blob_arg = Osc.(Message {
  address = "/test";
  arguments = [
    Blob "";
  ];
})

let message_all_args = Osc.(Message {
  address = "/test";
  arguments = [
    Blob "baz";
    String "quux";
    Int32 789l;
    Float32 123.456;
  ];
})

let test_packets_no_blobs = [
  "message_no_args", message_no_args;
  "message_empty_string_arg", message_empty_string_arg;
  "message_all_args_except_blobs", message_all_args_except_blobs;
]

let test_packets_with_blobs = [
  "message_empty_blob_arg", message_empty_blob_arg;
  "message_all_args", message_all_args;
]

let test_packets = test_packets_no_blobs @ test_packets_with_blobs

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

let check_results results =
  if List.exists
    OUnit.(function | RFailure _ | RError _ -> true | _ -> false)
    results
  then exit 1
  else exit 0
