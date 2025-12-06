open Core

let read_file filename = Stdio.In_channel.read_all filename |> String.rstrip
let read_lines filename = Stdio.In_channel.read_lines filename

let parse_ranges (data : string list) : (int * int) list =
  data
  |> List.map ~f:(fun s ->
         match String.split ~on:'-' s with
         | [ a; b ] -> (int_of_string a, int_of_string b)
         | _ -> failwith "Expected exactly two parts")

let split_on_substring s ~sep =
  match String.substr_index s ~pattern:sep with
  | None -> [ s ]
  | Some i ->
      let before = String.slice s 0 i in
      let after = String.slice s (i + String.length sep) (String.length s) in
      [ before; after ]
