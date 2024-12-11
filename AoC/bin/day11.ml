open Core
open AoC

let () =
  let filename = "input/Day11.txt" in
  let stones =
    Utils.read_file_as_string filename
    |> String.split ~on:' ' |> List.map ~f:Int.of_string
  in
  List.sum (module Int) stones ~f:(fun s -> Day11.blink s 25)
  |> sprintf "Part 1: Num stones = %d"
  |> print_endline;
  List.sum (module Int) stones ~f:(fun s -> Day11.blink s 75)
  |> sprintf "Part 2: Num stones = %d"
  |> print_endline
