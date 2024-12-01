open Core
open AoC

let () =
  let filename = "input/Day1.txt" in
  let xs, ys = Utils.read_file_as_split_ints filename in
  sprintf "Part 1: Total Distance = %d" (Day1.total_distance xs ys)
  |> print_endline;
  sprintf "Part 2: Total Similarity = %d" (Day1.similarity_score xs ys)
  |> print_endline
