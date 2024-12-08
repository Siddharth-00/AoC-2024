open Core

let is_out_of_bounds (i, j) ~size:(m, n) = i < 0 || i >= m || j < 0 || j >= n
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let get_antinodes p1 p2 =
  let d = Utils.Position.dist p1 p2 in
  let neg_d =
    let i, j = d in
    (-i, -j)
  in
  (Utils.Position.add p2 d, Utils.Position.add p1 neg_d)

let get_antinodes' p1 p2 ~size =
  let k, l = Utils.Position.dist p1 p2 in
  let gcd = gcd k l in
  let k', l' = (k / gcd, l / gcd) in
  let rec helper p d =
    let p' = Utils.Position.add p d in
    if is_out_of_bounds p' ~size then [] else p' :: helper p' d
  in
  List.concat [ [ p1 ]; helper p1 (k', l'); helper p1 (-k', -l') ]

let to_antenna_map grid =
  List.concat_mapi grid ~f:(fun i ->
      List.filter_mapi ~f:(fun j c ->
          if Char.is_alphanum c then Some (c, (i, j)) else None))
  |> Char.Map.of_alist_multi

let rec all_pairs = function
  | [] | [ _ ] -> []
  | x :: xs -> List.map xs ~f:(fun x' -> (x, x')) |> List.append (all_pairs xs)

let all_antinodes grid =
  let size = (List.length grid, List.length (List.hd_exn grid)) in
  let antennas = to_antenna_map grid in
  let antenna_pairs = Map.map antennas ~f:all_pairs in
  Map.map antenna_pairs
    ~f:
      (List.concat_map ~f:(fun (p1, p2) ->
           let a1, a2 = get_antinodes p1 p2 in
           List.filter [ a1; a2 ] ~f:(Fn.non (is_out_of_bounds ~size))))
  |> Map.data |> List.concat |> Utils.Position.Set.of_list

let all_antinodes' grid =
  let size = (List.length grid, List.length (List.hd_exn grid)) in
  let antennas = to_antenna_map grid in
  let antenna_pairs = Map.map antennas ~f:all_pairs in
  Map.map antenna_pairs
    ~f:(List.concat_map ~f:(fun (p1, p2) -> get_antinodes' p1 p2 ~size))
  |> Map.data |> List.concat |> Utils.Position.Set.of_list
