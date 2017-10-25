open Osc
open OUnit

let message_no_args = Types.(Message {
  address = "/test";
  arguments = [];
})

let message_empty_string_arg = Types.(Message {
  address = "/test";
  arguments = [
    String "";
  ];
})

let message_all_args_except_blobs = Types.(Message {
  address = "/test";
  arguments = [
    String "foobar";
    Int32 123l;
    Float32 456.789;
  ];
})

let message_empty_blob_arg = Types.(Message {
  address = "/test";
  arguments = [
    Blob "";
  ];
})

let message_all_args = Types.(Message {
  address = "/test";
  arguments = [
    Blob "baz";
    String "quux";
    Int32 789l;
    Float32 123.456;
  ];
})

let bundle_immediate_no_packets = Types.(Bundle {
  timetag = Immediate;
  packets = [];
})

let bundle_no_packets = Types.(Bundle {
  timetag = Time {seconds = 147l; fraction = 258l};
  packets = [];
})

let bundle_one_message = Types.(Bundle {
  timetag = Time {seconds = 12l; fraction = 48l};
  packets = [message_all_args];
})

let bundle_two_messages = Types.(Bundle {
  timetag = Immediate;
  packets = [message_all_args; message_empty_blob_arg];
})

let bundle_recursive = Types.(Bundle {
  timetag = Time {seconds = 678l; fraction = 345l};
  packets = [message_all_args; bundle_two_messages];
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

let test_bundles = [
  "bundle_immediate_no_packets", bundle_immediate_no_packets;
  "bundle_no_packets", bundle_no_packets;
  "bundle_one_message", bundle_one_message;
  "bundle_two_messages", bundle_two_messages;
  "bundle_recursive", bundle_recursive;
]

let test_packets =
  test_packets_no_blobs @ test_packets_with_blobs @ test_bundles

let are_arguments_equal arg1 arg2 =
  let open Types in
  match arg1, arg2 with
  | Blob a, Blob b -> a = b
  | String a, String b -> a = b
  | Int32 a, Int32 b -> a = b
  | Float32 a, Float32 b ->
    let diff = abs_float (a -. b) in
    diff <= 0.01
  | _, _ -> false

let string_of_argument = function
  | Types.Blob s -> Printf.sprintf "Blob %s" s
  | Types.String s -> Printf.sprintf "String %s" s
  | Types.Int32 i -> Printf.sprintf "Int32 %ld" i
  | Types.Float32 f -> Printf.sprintf "Float32 %f" f
  | Types.Timetag t ->
    Printf.sprintf "Timetag %s"
      (match t with
      | Types.Immediate -> "Immediate"
      | Types.Time time ->
        Printf.sprintf "%ld.%ld"
          time.Types.seconds time.Types.fraction)

let assert_messages_equal message1 message2 =
  let open Osc.Types in
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

let rec assert_bundles_equal bundle1 bundle2 =
  let open Osc.Types in
  assert_equal
    ~msg:"Incorrect timetag"
    bundle1.timetag bundle2.timetag;
  assert_equal
    ~msg:"Wrong number of packets in bundle"
    (List.length bundle1.packets)
    (List.length bundle2.packets);
  List.iter2
    (fun packet1 packet2 -> assert_packets_equal packet1 packet2)
    bundle1.packets bundle2.packets

and assert_packets_equal packet1 packet2 =
  let open Osc.Types in
  match packet1, packet2 with
  | Message message1, Message message2 ->
    assert_messages_equal message1 message2
  | Bundle bundle1, Bundle bundle2 ->
    assert_bundles_equal bundle1 bundle2
  | _, _ ->
    assert_failure "Packet types differ"

let check_results results =
  if List.exists
    OUnit.(function | RFailure _ | RError _ -> true | _ -> false)
    results
  then exit 1
  else exit 0

let run suite =
  check_results (run_test_tt suite)
