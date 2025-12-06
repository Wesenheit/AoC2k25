open Core
open Util

let separate_input (filename : string) : string * string =
  let data = Stdio.In_channel.read_all filename in
  let list = split_on_substring data ~sep:"\n\n" in
  let out =
    match list with
    | [ a; b ] -> (a, b)
    | _ -> failwith "more than one separating substrings"
  in
  out

let merge_intervals (intervals : (int * int) list) : (int * int) list =
  let sorted =
    List.sort ~compare:(fun (a1, _) (a2, _) -> compare a1 a2) intervals
  in
  let merge acc (a, b) =
    match acc with
    | [] -> [ (a, b) ]
    | (a0, b0) :: rest ->
        if a <= b0 then (a0, max b0 b) :: rest else (a, b) :: acc
  in
  sorted |> List.fold ~f:merge ~init:[] |> List.rev

let find_if_valid (intervals : (int * int) list) (x : int) : bool =
  let rec aux left right =
    if left > right then false
    else
      let mid = (left + right) / 2 in
      let a, b = Option.value_exn (List.nth intervals mid) in
      if x < a then aux left (mid - 1)
      else if x > b then aux (mid + 1) right
      else true
  in
  aux 0 (List.length intervals - 1)

let () =
  let data = separate_input "data/day5.txt" in
  let ranges = fst data in
  let queries = snd data in
  let parsed_ranges =
    ranges |> String.split_on_chars ~on:[ '\n' ] |> parse_ranges
  in
  let parsed_queries =
    String.split_lines queries |> List.map ~f:int_of_string
  in
  let merged_intervals = merge_intervals parsed_ranges in
  let () =
    let final_val =
      List.fold
        ~f:(fun acc value ->
          if find_if_valid merged_intervals value then acc + 1 else acc)
        parsed_queries ~init:0
    in
    Stdio.printf "Final val (part one): %d \n" final_val
  in
  let () =
    let sumy =
      List.fold
        ~f:(fun acc (a, b) -> acc + (b - a) + 1)
        ~init:0 merged_intervals
    in
    Stdio.printf "Final val (part two): %d \n" sumy
  in
  ()
