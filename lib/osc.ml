type argument =
  | Int32 of int32
  | Float32 of float
  | Str of string
  | Blob of string

type message = {
  address: string;
  arguments: argument list;
}

type packet =
  | Message of message
  | Bundle of packet list

let blob_padding_of_length length =
  match length mod 4 with
  | 0 -> 0
  | x -> 4 - x

let encode_float f =
  BITSTRING {
    Int32.bits_of_float f : 32 : bigendian
  }

let encode_int32 i =
  BITSTRING {
    i : 32 : bigendian
  }

let encode_string s =
  (* Add nulls to pad the string length out to a multiple of
   * four bytes, then convert to a bitstring. *)
  let length = String.length s in
  let padding = 4 - (length mod 4) in
  let suffix = String.make padding '\000' in
  Bitstring.bitstring_of_string (s ^ suffix)

let encode_blob b =
  let length = String.length b in
  let padding = blob_padding_of_length length in
  if padding = 0 then
    BITSTRING {
      (Int32.of_int length) : 32 : bigendian;
      b : length * 8 : string
    }
  else
    let suffix = String.make padding '\000' in
    BITSTRING {
      (Int32.of_int length) : 32 : bigendian;
      b : length * 8 : string;
      suffix : padding * 8 : string
    }

let read_float data =
  bitmatch data with {
    i : 4*8 : bigendian;
    rest : -1 : bitstring
  } ->
    Int32.float_of_bits i, rest

let read_int32 data =
  bitmatch data with {
    i : 4*8 : bigendian;
    rest : -1 : bitstring
  } -> i, rest

let read_string data =
  let rec read_string' buffer data =
    bitmatch data with {
      s : 4*8 : string;
      rest : -1 : bitstring
    } ->
      try
        (* If there's a null, we're at the end. Append everything up the
         * first zero to the buffer, and return the buffer contents. *)
        let index = String.index s '\000' in
        Buffer.add_string buffer (String.sub s 0 index);
        Buffer.contents buffer, rest
      with Not_found ->
        (* No null found, so append all four bytes to the buffer and
         * recursively look at the next four bytes in the bitstring. *)
        Buffer.add_string buffer s;
        read_string' buffer rest
  in
  read_string' (Buffer.create 16) data

let read_blob data =
  let size, rest = read_int32 data in
  let blob_length = Int32.to_int size in
  let padding_length = blob_padding_of_length blob_length in
  bitmatch rest with {
    b : blob_length * 8 : string;
    _ : padding_length * 8 : string;
    rest : -1 : bitstring
  } -> b, rest
