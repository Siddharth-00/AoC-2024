open Core
open AoC

let () =
  let filename = "input/Day8.txt" in
  let grid = Utils.read_file_as_2d_char_matrix filename in
  Day8.all_antinodes grid |> Set.length
  |> sprintf "Part 1: Num of Antinodes = %d"
  |> print_endline;
  Day8.all_antinodes' grid |> Set.length
  |> sprintf "Part 2: Num of Antinodes = %d"
  |> print_endline
