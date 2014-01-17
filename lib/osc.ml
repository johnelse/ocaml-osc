type argument =
  | Float32 of float
  | Int32 of int32
  | Str of string
  | Blob of string

type message = {
  address: string;
  arguments: argument list;
}

type time = {
  seconds: int32;
  fraction: int32;
}

type timetag =
  | Immediate
  | Time of time

type packet =
  | Message of message
  | Bundle of (timetag * packet list)
