open OUnit

type test_config = {
  ml_port: int;
  sclang_path: string;
  sc_port: int;
  sc_script_path: string;
}

let ping_sclang config packet =
  let open Osc in
  let open Osc_unix.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let ml_addr = Unix.ADDR_INET (localhost, config.ml_port) in
  let sc_addr = Unix.ADDR_INET (localhost, config.sc_port) in
  let sent_packet =
    match packet with
    | Message _ -> packet
    | Bundle _ -> failwith "Bundles not implemented"
  in
  bracket
    (fun () ->
      match Unix.fork () with
      | 0 ->
        (* Child -> will exec the SuperCollider echo server. *)
        Unix.execv
          config.sclang_path
          [|
            config.sclang_path;
            "-u";
            string_of_int config.sc_port;
            config.sc_script_path;
          |]
      | child_pid ->
        Printf.printf "ocaml: forked sclang with pid %d\n%!" child_pid;
        let client = Client.create () in
        let server = Server.create ml_addr 4096 in
        Printf.printf "ocaml: waiting 5 seconds for sclang to start up\n%!";
        Unix.sleep 5;
        (child_pid, client, server))
    (fun (_, client, server) ->
      Printf.printf "ocaml: sending packet to port %d\n%!" config.sc_port;
      Client.send client sc_addr sent_packet;
      Printf.printf "ocaml: packet sent\n%!";
      let (received_packet, _) = Server.recv server in
      Printf.printf "ocaml: packet received\n%!";
      Test_common.assert_packets_equal sent_packet received_packet)
    (fun (child_pid, client, server) ->
      Printf.printf "ocaml: killing sclang\n%!";
      Unix.kill child_pid Sys.sigterm;
      Client.destroy client;
      Server.destroy server)
    ()

let test_ping_sclang config =
  "test_ping_sclang" >::: (
    List.map
      (fun (name, packet) ->
        name >:: (fun () -> ping_sclang config packet))
      (* TODO: supercollider's handling of blobs via OSC is a bit suspect,
       *       so drop the test packets containing blobs for now. *)
      Test_common.test_packets_no_blobs
  )

let test_interop_sclang config =
  "test_interop_sclang" >:::
    [
      test_ping_sclang config;
    ]

(* Store the port on which OCaml will listen on in a file. *)
let write_ml_port ml_port =
  let data_file_path = "test.data" in
  let chan = open_out data_file_path in
  begin
    try output_binary_int chan ml_port;
    with e ->
      close_out chan;
      raise e
  end;
  close_out chan

let usage () =
  Printf.printf "Usage:\n%!";
  Printf.printf
    "%s <ml-port> <sclang-path> <sc-port> <sc-script-path>\n%!"
    Sys.executable_name

let () =
  match Sys.argv with
  | [|_; ml_port_string; sclang_path; sc_port_string; sc_script_path|] -> begin
    try
      let ml_port = int_of_string ml_port_string in
      let sc_port = int_of_string sc_port_string in
      write_ml_port ml_port;
      print_endline "-------- SuperCollider interoperability tests --------";
      let config = {ml_port; sclang_path; sc_port; sc_script_path} in
      Test_common.run (test_interop_sclang config)
    with (Failure "int_of_string") -> usage ()
  end
  | _ -> usage ()
