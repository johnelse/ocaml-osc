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

let read_float data =
  bitmatch data with {
    i : 4*8 : bigendian;
    rest : -1 : bitstring
  } ->
    Int32.float_of_bits i, rest

let read_int data =
  bitmatch data with {
    i : 4*8 : bigendian;
    rest : -1 : bitstring
  } -> i, rest
