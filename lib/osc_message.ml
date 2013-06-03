type argument =
  | Int32 of int32
  | Float32 of float
  | Str of string
  | Blob of string

let bits_of_float f =
  BITSTRING {
    Int32.bits_of_float f : 32 : bigendian
  }

let float_of_bits b =
  bitmatch b with {
    i : 4*8 : bigendian
  } ->
    Int32.float_of_bits i
