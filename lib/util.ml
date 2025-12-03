open Core

let read_file filename = Stdio.In_channel.read_all filename |> String.rstrip
let read_lines filename = Stdio.In_channel.read_lines filename
