open Core

let read_file_as_string = In_channel.read_all
let read_file_lines s = In_channel.read_lines s

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

let read_file_as_2d_int_matrix filename =
  read_file_lines filename
  |> List.map ~f:(fun s -> String.split ~on:' ' s |> List.map ~f:Int.of_string)

let read_file_as_2d_char_matrix filename =
  read_file_lines filename |> List.map ~f:String.to_list

let print_2d_matrix grid ~f =
  List.map grid ~f:(List.to_string ~f) |> List.iter ~f:print_endline

let rec intersperce y ys =
  match ys with
  | [] -> [ [ y ] ]
  | y' :: ys' ->
      (y :: y' :: ys') :: List.map (intersperce y ys') ~f:(fun zs -> y' :: zs)

let rec permutations = function
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | x :: xs ->
      permutations xs
      |> List.map ~f:(fun perm -> intersperce x perm)
      |> List.concat

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let add (i, j) (i', j') = (i + i', j + j')
  let dist (i, j) (i', j') = (i' - i, j' - j)
end
