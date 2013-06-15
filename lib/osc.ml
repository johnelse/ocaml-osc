type argument =
  | Int32 of int32
  | Float32 of float
  | Str of string
  | Blob of string

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

