type time = {
  seconds: int32;
  fraction: int32;
}

type timetag =
  | Immediate
  | Time of time

type argument =
  | Float32 of float
  | Int32 of int32
  | String of string
  | Blob of string
  | Timetag of timetag

type message = {
  address: string;
  arguments: argument list;
}

type bundle = {
  timetag: timetag;
  packets: packet list;
}

and packet =
  | Message of message
  | Bundle of bundle
