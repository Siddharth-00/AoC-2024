open Core
open AoC

let () =
  let filename = "input/Day2.txt" in
  let xxs = Utils.read_file_as_2d_matrix filename in

  sprintf "Num safe reports = %d"
    (List.sum (module Int) (AoC.Day2.reports_safe xxs) ~f:Bool.to_int)
  |> print_endline;

  sprintf "Num safe reports with dampener = %d"
    (List.sum
       (module Int)
       (AoC.Day2.reports_safe_with_dampener xxs)
       ~f:Bool.to_int)
  |> print_endline
