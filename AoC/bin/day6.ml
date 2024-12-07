open Core
open AoC

let () =
  let filename = "input/Day6.txt" in
  let grid =
    Utils.read_file_as_2d_char_matrix filename
    |> List.map ~f:Array.of_list |> Array.of_list
  in
  Day6.num_distinct_positions grid
  |> sprintf "Part 1: Num Unique Positions = %d"
  |> print_endline;
  Day6.num_blockages_with_cycle grid
  |> sprintf "Part 2: Num Blockages with Cycles = %d"
  |> print_endline
