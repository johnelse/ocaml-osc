ocaml-osc
=========

[![Build status](https://travis-ci.org/johnelse/ocaml-osc.png?branch=master)](https://travis-ci.org/johnelse/ocaml-osc)
[![Coverage Status](https://coveralls.io/repos/johnelse/ocaml-osc/badge.svg?branch=master&service=github)](https://coveralls.io/github/johnelse/ocaml-osc?branch=master)
[![API reference](https://img.shields.io/badge/docs-API_reference-blue.svg)](https://johnelse.github.io/ocaml-osc)

Pure OCaml implementation of the
[Open Sound Control](http://opensoundcontrol.org/) protocol.

Dependencies:

* [lwt](http://ocsigen.org/lwt/) (optional)
* [ocplib-endian](https://github.com/OCamlPro/ocplib-endian)
* [oUnit](http://ounit.forge.ocamlcore.org/)
* [rresult](https://github.com/dbuenzli/rresult)

Usage
=====

If you just need to parse and serialise OSC packets, use the `Osc.Codec`
module:

``` ocaml
# require "osc";;

# open Osc.Types;;

# let data = Osc.Codec.of_packet (Message {address = "/hello/world"; arguments = [Int32 123l; String "foo"]});;
val data : bytes = "/hello/world\000\000\000\000,is\000\000\000\000{foo\000"

# let packet = Osc.Codec.to_packet data;;
val packet : (packet, [ `Missing_typetag_string | `Unsupported_typetag of char ]) Rresult.result =
  Rresult.Ok (Message {address = "/hello/world"; arguments = [Int32 123l; String "foo"]})
```

To simplify sending and receiving OSC packets over the network, the modules
`Osc_lwt` and `Osc_unix` are available:

``` ocaml
# require "osc.lwt";;

# require "lwt.syntax";;

# open Osc.Types;;

# lwt client = Osc_lwt.Udp.Client.create ();;
val client : Osc_lwt.Udp.Client.t = <abstr>

# let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 57120);;
val addr : Lwt_unix.sockaddr = Unix.ADDR_INET (<abstr>, 57120)

# lwt () = Osc_lwt.Udp.Client.send client addr (Message {address = "/hello/world"; arguments = [Int32 123l; String "foo"]});;
```
