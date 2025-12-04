open Core

let find_max (s : string) : int =
  let int_values =
    List.init (String.length s) ~f:(fun i ->
        int_of_char s.[i] - int_of_char '0')
  in
  let init_val = (-100, -100) in
  let acc_fun =
   fun x (max_sum, max) ->
    let new_max = if x > max then x else max in
    let proposed_max = (10 * x) + max in
    let new_max_sum =
      if proposed_max > max_sum then proposed_max else max_sum
    in
    (new_max_sum, new_max)
  in
  fst (List.fold_right int_values ~f:acc_fun ~init:init_val)

let sum_max_batteries (batteries : string list) : int =
  List.fold batteries ~init:0 ~f:(fun acc x ->
      let max = find_max x in
      acc + max)

let () =
  let batteries = Util.read_lines "data/day3.txt" in
  let () =
    let final_val_one = sum_max_batteries batteries in
    Stdio.printf "Final val (part one): %d \n" final_val_one
  in
  ()
