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

let read_file_as_2d_int_matrix_split_with_spaces filename =
  read_file_lines filename
  |> List.map ~f:(fun s -> String.split ~on:' ' s |> List.map ~f:Int.of_string)

let read_file_as_2d_int_matrix filename =
  read_file_lines filename
  |> List.map ~f:(fun s ->
         String.to_list s
         |> List.map ~f:(fun c -> Char.to_int c - Char.to_int '0'))

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
    type t = int * int [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let add (i, j) (i', j') = (i + i', j + j')
  let dist (i, j) (i', j') = (i' - i, j' - j)
end

module type Arg = sig
  type t [@@deriving sexp, compare, hash]
end

let memoize2_recursive (type a b c) (module T : Arg with type t = a)
    (module T' : Arg with type t = b) f =
  let module T'' = Hashable.Make (struct
    type t = T.t * T'.t [@@deriving sexp, compare, hash]
  end) in
  let cache = T''.Table.create () in
  let rec memoized x y =
    match Hashtbl.find cache (x, y) with
    | Some res -> res
    | None ->
        let res = f memoized x y in
        let _ = Hashtbl.add cache ~key:(x, y) ~data:res in
        res
  in
  memoized

let memoize2 (type a b c) (module T : Arg with type t = a)
    (module T' : Arg with type t = b) f =
  memoize2_recursive (module T) (module T') (fun _recurse -> f)

let memoize_recursive (type a b c) (module T : Arg with type t = a) f =
  memoize2_recursive
    (module Unit)
    (module T)
    (fun recurse () x -> f (recurse ()) x)
    ()

let memoize (type a b) (module T : Arg with type t = a) f =
  memoize_recursive (module T) (fun _recurse -> f)
