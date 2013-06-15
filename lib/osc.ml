type argument =
  | Int32 of int32
  | Float32 of float
  | Str of string
  | Blob of string

let encode_float f =
  BITSTRING {
    Int32.bits_of_float f : 32 : bigendian
  }

let encode_int i =
  BITSTRING {
    i : 32 : bigendian
  }

let decode_float b =
  bitmatch b with {
    i : 4*8 : bigendian
  } ->
    Int32.float_of_bits i

let decode_int b =
  bitmatch b with {
    i : 4*8 : bigendian
  } -> i
