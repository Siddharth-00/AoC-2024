open Core

let total_distance xs ys =
  let xs_sorted = List.sort xs ~compare:Int.compare in
  let ys_sorted = List.sort ys ~compare:Int.compare in
  List.zip_exn xs_sorted ys_sorted
  |> List.map ~f:(fun (x, y) -> abs (x - y))
  |> List.sum (module Int) ~f:Fn.id

let similarity_score xs ys =
  let counts =
    List.sort_and_group ys ~compare:Int.compare
    |> List.map ~f:(fun zs -> (List.hd_exn zs, List.length zs))
    |> Int.Map.of_alist_exn
  in
  List.map xs ~f:(fun x -> x * (Map.find counts x |> Option.value ~default:0))
  |> List.sum (module Int) ~f:Fn.id
