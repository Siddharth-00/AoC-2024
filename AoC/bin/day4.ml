open Core
open AoC

let () =
  let filename = "input/Day4.txt" in
  let char_grid = Utils.read_file_as_2d_char_matrix filename in
  Day4.find_xmas char_grid |> sprintf "Part 1: Num XMAS = %d" |> print_endline;
  Day4.find_x_mas char_grid |> sprintf "Part 2: Num X_MAS = %d" |> print_endline
