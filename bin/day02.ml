open Base
open Core
open Util

let find_if_duplicate (id : string) : bool =
  let n = String.length id in
  if n % 2 <> 0 then false
  else
    let half = n / 2 in
    let first = String.sub id ~pos:0 ~len:half in
    let second = String.sub id ~pos:half ~len:half in
    String.equal first second

let composed_of_n_substrings ((s, n) : string * int) : bool =
  let len = String.length s in
  if len mod n <> 0 then false
  else
    let sub_len = len / n in
    let sub = String.sub s ~pos:0 ~len:sub_len in
    let rec check i =
      if i = n then true
      else
        let start = i * sub_len in
        if String.equal (String.sub s ~pos:start ~len:sub_len) sub then
          check (i + 1)
        else false
    in
    check 0

let sum_if_repeated (id : int) : int =
  let s = string_of_int id in
  let len = String.length s in
  let rec aux n =
    if n > len then None
    else if len mod n = 0 && composed_of_n_substrings (s, n) then Some n
    else aux (n + 1)
  in
  let first_n = aux 2 in
  match first_n with None -> 0 | Some _ -> id

let sum_duplicate (id : int) : int =
  if find_if_duplicate (string_of_int id) then id else 0

let () =
  let parsed_ranges =
    read_file "data/day2.txt"
    |> String.split_on_chars ~on:[ ',' ]
    |> parse_ranges
  in
  let all_values =
    parsed_ranges |> List.concat_map ~f:(fun (a, b) -> List.range a (b + 1))
  in
  let () =
    let final_val_one =
      List.fold_left all_values ~f:(fun acc x -> sum_duplicate x + acc) ~init:0
    in
    Stdio.printf "Final val (part one): %d \n" final_val_one
  in
  let () =
    let final_val_two =
      List.fold_left all_values
        ~f:(fun acc x -> sum_if_repeated x + acc)
        ~init:0
    in
    Stdio.printf "Final val (part two): %d \n" final_val_two
  in
  ()
