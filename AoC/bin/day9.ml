open Core
open AoC

let () =
  let filename = "input/Day9.txt" in
  let blocks =
    Utils.read_file_as_string filename
    |> String.to_list
    |> List.map ~f:(fun c -> Char.to_int c - Char.to_int '0')
  in
  let expanded_blocks = Day9.expand_blocks blocks in
  Day9.compress_and_get_checksum ~expanded_blocks
  |> sprintf "Part 1: Checksum = %d"
  |> print_endline;
  Day9.compress_continuous_and_get_checksum blocks
  |> sprintf "Part 2: Whole File Checksum = %d"
  |> print_endline
