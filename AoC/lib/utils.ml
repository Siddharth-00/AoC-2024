open Core

let read_file_lines = In_channel.read_lines

let read_file_as_ints filename =
  read_file_lines filename |> List.map ~f:Int.of_string

let read_file_as_split_ints filename =
  read_file_lines filename
  |> List.map ~f:(fun s ->
         match
           String.split ~on:' ' s |> List.filter ~f:(Fn.non String.is_empty)
         with
         | [ x; y ] -> (Int.of_string x, Int.of_string y)
         | _ -> failwithf "Malformed line: %s" s ())
  |> List.unzip
