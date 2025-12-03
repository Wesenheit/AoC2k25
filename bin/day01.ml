open Base
open Util

type direction = R | L
type instruction = { direction : direction; distance : int }

let parse_line line : instruction =
  let direction_char = line.[0] in
  let distance_str = String.sub line ~pos:1 ~len:(String.length line - 1) in
  let distance =
    match Int.of_string_opt distance_str with
    | Some d -> d
    | None -> failwith "distance err"
  in

  let direction =
    match direction_char with
    | 'R' -> R
    | 'L' -> L
    | _ -> failwith "direction err"
  in
  { direction; distance }

let update_part_one ((current_val, zeros) : int * int) (element : instruction) :
    int * int =
  let sign = match element.direction with R -> 1 | L -> -1 in
  let next_val = Int.rem (current_val + (sign * element.distance)) 100 in
  let next_zeros = match next_val with 0 -> zeros + 1 | _ -> zeros in
  (next_val, next_zeros)

let positive_modulo a n =
  let result = Int.rem a n in
  if result >= 0 then result else result + n

let update_part_two ((current_val, zeros) : int * int) (element : instruction) :
    int * int =
  let sign = match element.direction with R -> 1 | L -> -1 in
  let next_val = current_val + (sign * Int.rem element.distance 100) in
  let next_val_rem = positive_modulo next_val 100 in
  let full_cycle = element.distance / 100 in
  let partial_cycle =
    if element.distance > 0 && current_val <> 0 then
      let crossed_zero = next_val >= 100 || next_val <= 0 in
      if crossed_zero then 1 else 0
    else 0
  in
  let new_total_zeros = zeros + full_cycle + partial_cycle in
  (next_val_rem, new_total_zeros)

let () =
  let files = read_lines "data/day1.txt" in
  let directions = List.map files ~f:parse_line in

  let () =
    let final_state_part_one =
      List.fold_left ~init:(50, 0) ~f:update_part_one directions
    in
    let final_one = snd final_state_part_one in
    Stdio.printf "Final Code (part one): %d\n" final_one
  in

  let () =
    let final_state_part_two =
      List.fold_left ~init:(50, 0) ~f:update_part_two directions
    in
    let final_two = snd final_state_part_two in
    Stdio.printf "Final Code (part two): %d\n" final_two
  in
  ()
