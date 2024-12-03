open AoC
open Core

let () =
  let filename = "input/Day3.txt" in
  let s = Utils.read_file_as_string filename in
  let valid_muls = Day3.uncorrupt s in
  List.sum (module Int) valid_muls ~f:(fun (x, y) -> x * y)
  |> Int.to_string
  |> sprintf "Part 1: Sum of uncorrupt muls = %s"
  |> print_endline;
  let valid_muls_with_conditionals = Day3.uncorrupt_with_conditionals s in
  List.sum (module Int) valid_muls_with_conditionals ~f:(fun (x, y) -> x * y)
  |> Int.to_string
  |> sprintf "Part 2: Sum of uncorrupt muls with conditionals = %s"
  |> print_endline
