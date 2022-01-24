open OUnit

let message_no_args = Osc.Types.(Message {
  address = "/test";
  arguments = [];
})

let message_empty_string_arg = Osc.Types.(Message {
  address = "/test";
  arguments = [
    String "";
  ];
})

let message_all_args_basic = Osc.Types.(Message {
  address = "/test";
  arguments = [
    String "foobar";
    Int32 123l;
    Float32 456.789;
  ];
})

let message_empty_blob_arg = Osc.Types.(Message {
  address = "/test";
  arguments = [
    Blob "";
  ];
})

let message_timetag_immediate_arg = Osc.Types.(Message {
  address = "/test";
  arguments = [
    Timetag Immediate;
  ];
})

let message_timetag_time_arg = Osc.Types.(Message {
  address = "/test";
  arguments = [
    Timetag (Time {seconds = 456l; fraction = 123l});
  ];
})

let message_all_args = Osc.Types.(Message {
  address = "/test";
  arguments = [
    Blob "baz";
    String "quux";
    Int32 789l;
    Float32 123.456;
    Timetag Immediate;
    Timetag (Time {seconds = 456l; fraction = 123l});
  ];
})

let bundle_immediate_no_packets = Osc.Types.(Bundle {
  timetag = Immediate;
  packets = [];
})

let bundle_no_packets = Osc.Types.(Bundle {
  timetag = Time {seconds = 147l; fraction = 258l};
  packets = [];
})

let bundle_one_message = Osc.Types.(Bundle {
  timetag = Time {seconds = 12l; fraction = 48l};
  packets = [message_all_args];
})

let bundle_two_messages = Osc.Types.(Bundle {
  timetag = Immediate;
  packets = [message_all_args; message_empty_blob_arg];
})

let bundle_recursive = Osc.Types.(Bundle {
  timetag = Time {seconds = 678l; fraction = 345l};
  packets = [message_all_args; bundle_two_messages];
})

let test_packets_basic = [
  "message_no_args", message_no_args;
  "message_empty_string_arg", message_empty_string_arg;
  "message_all_args_basic", message_all_args_basic;
]

let test_packets_extended = [
  "message_empty_blob_arg", message_empty_blob_arg;
  "message_timetag_immediate_arg", message_timetag_immediate_arg;
  "message_timetag_time_arg", message_timetag_time_arg;
  "message_all_args", message_all_args;
]

let test_bundles = [
  "bundle_immediate_no_packets", bundle_immediate_no_packets;
  "bundle_no_packets", bundle_no_packets;
  "bundle_one_message", bundle_one_message;
  "bundle_two_messages", bundle_two_messages;
  "bundle_recursive", bundle_recursive;
]

let test_packets_sclang = test_packets_basic

let test_packets_internal =
  test_packets_basic @ test_packets_extended @ test_bundles

let are_arguments_equal arg1 arg2 =
  let open Osc.Types in
  match arg1, arg2 with
  | Blob a, Blob b -> a = b
  | String a, String b -> a = b
  | Int32 a, Int32 b -> a = b
  | Float32 a, Float32 b ->
    let diff = abs_float (a -. b) in
    diff <= 0.01
  | Timetag a, Timetag b -> a = b
  | _, _ -> false

let string_of_argument = function
  | Osc.Types.Blob s -> Printf.sprintf "Blob %s" s
  | Osc.Types.String s -> Printf.sprintf "String %s" s
  | Osc.Types.Int32 i -> Printf.sprintf "Int32 %ld" i
  | Osc.Types.Float32 f -> Printf.sprintf "Float32 %f" f
  | Osc.Types.Timetag t ->
    Printf.sprintf "Timetag %s"
      (match t with
      | Osc.Types.Immediate -> "Immediate"
      | Osc.Types.Time time ->
        Printf.sprintf "%ld.%ld"
          time.Osc.Types.seconds time.Osc.Types.fraction)

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
