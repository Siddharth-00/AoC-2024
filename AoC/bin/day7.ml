open Core
open AoC

let () =
  let filename = "input/Day7.txt" in
  let lines = Utils.read_file_lines filename in
  let input = Day7.parse_input lines in
  (* Day7.valid_test_values input
     |> Map.keys
     |> List.sum (module Int) ~f:Fn.id
     |> sprintf "Part 1: Total of valid test values = %d"
     |> print_endline *)
  Day7.valid_test_values input
  |> Map.keys
  |> List.sum (module Int) ~f:Fn.id
  |> sprintf "Part 2: Total of valid test values = %d"
  |> print_endline
