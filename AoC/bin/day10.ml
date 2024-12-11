open Core
open AoC

let () =
  let filename = "input/Day10.txt" in
  let grid =
    Utils.read_file_as_2d_int_matrix filename
    |> List.map ~f:Array.of_list |> Array.of_list
  in
  Day10.trailhead_scores_sum grid
  |> sprintf "Part 1: Sum of trailhead scores = %d"
  |> print_endline;
  Day10.trailhead_rating_sum grid
  |> sprintf "Part 2: Sum of trailhead rating = %d"
  |> print_endline
