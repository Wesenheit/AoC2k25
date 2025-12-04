open Core

let char_to_int (c : char) : int =
  match c with '.' -> 0 | '@' -> 1 | _ -> failwith "Invalid character"

let parse_line (line : string) : int array =
  let len = String.length line in
  Array.init len ~f:(fun i -> char_to_int (String.get line i))

let read_grid (filename : string) : int array array =
  Util.read_lines filename |> List.map ~f:parse_line |> Array.of_list

let conv2d_3x3_padded grid =
  let n = Array.length grid in
  let output = Array.make_matrix ~dimx:n ~dimy:n 0 in

  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let sum = ref 0 in
      for u = -1 to 1 do
        for v = -1 to 1 do
          let x = i + u in
          let y = j + v in
          if x >= 0 && x < n && y >= 0 && y < n then sum := !sum + grid.(x).(y)
        done
      done;
      output.(i).(j) <- !sum - grid.(i).(j)
    done
  done;
  output

let flatten_grid grid =
  grid |> Array.to_list |> List.concat_map ~f:Array.to_list

let () =
  let parsed_grid = read_grid "data/day4.txt" in
  let sum_grid = conv2d_3x3_padded parsed_grid in
  let zipped =
    List.zip_exn (flatten_grid parsed_grid) (flatten_grid sum_grid)
  in
  let accesible = List.filter zipped ~f:(fun (g, s) -> g <> 0 && s < 4) in
  let () =
    let final_val = List.length accesible in
    Stdio.printf "Final value (part one): %d \n" final_val
  in
  ()
