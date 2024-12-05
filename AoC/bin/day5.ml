open Core
open AoC

let () =
  let filename = "input/Day5.txt" in
  let text_input = Utils.read_file_as_string filename in
  Day5.middle_pages_from_valid_updates text_input
  |> List.sum (module Int) ~f:Fn.id
  |> sprintf "Part 1: Sum of valid middle pages = %d"
  |> print_endline;

  Day5.middle_pages_from_invalid_updates_after_fixing text_input
  |> List.sum (module Int) ~f:Fn.id
  |> sprintf "Part 2: Sum of invalid middle pages after fixing = %d"
  |> print_endline
