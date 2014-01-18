open OUnit

let test_message = Osc.({
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

let test_send_message () =
  bracket
    (fun () ->
      let infd, outfd = Unix.pipe () in
      let inchan = Unix.in_channel_of_descr infd in
      let outchan = Unix.out_channel_of_descr outfd in
      inchan, outchan)
    (fun (inchan, outchan) ->
      Osc_unix.Codec.(
        Encode.message outchan test_message;
        let received_message = Decode.message inchan in
        assert_messages_equal test_message received_message
      ))
    (fun (inchan, outchan) ->
      close_out outchan;
      close_in inchan)
    ()

let base_suite =
  "base_suite" >:::
    [
      "test_send_message" >:: test_send_message;
    ]

let _ = run_test_tt_main base_suite
