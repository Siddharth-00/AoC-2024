open Core

let find_trailheads grid =
  Array.foldi grid ~init:[] ~f:(fun i trailheads ->
      Array.foldi ~init:trailheads ~f:(fun j trailheads' n ->
          if n = 0 then (i, j) :: trailheads' else trailheads'))

let trailhead_rating grid trailhead =
  let m, n = (Array.length grid, Array.length grid.(0)) in
  let in_bounds (i, j) = i >= 0 && i < m && j >= 0 && j < n in
  let adjacent (i, j) =
    List.filter [ (i + 1, j); (i, j + 1); (i - 1, j); (i, j - 1) ] ~f:in_bounds
  in
  let rec helper (i, j) =
    if grid.(i).(j) = 9 then 1
    else
      List.sum
        (module Int)
        (adjacent (i, j))
        ~f:(fun (i', j') ->
          if grid.(i').(j') = grid.(i).(j) + 1 then helper (i', j') else 0)
  in
  helper trailhead

let trailhead_score grid trailhead =
  let m, n = (Array.length grid, Array.length grid.(0)) in
  let in_bounds (i, j) = i >= 0 && i < m && j >= 0 && j < n in
  let adjacent (i, j) =
    List.filter [ (i + 1, j); (i, j + 1); (i - 1, j); (i, j - 1) ] ~f:in_bounds
  in
  let rec helper (i, j) =
    if grid.(i).(j) = 9 then Utils.Position.Set.of_list [ (i, j) ]
    else
      List.map
        (adjacent (i, j))
        ~f:(fun (i', j') ->
          if grid.(i').(j') = grid.(i).(j) + 1 then helper (i', j')
          else Utils.Position.Set.empty)
      |> Utils.Position.Set.union_list
  in
  helper trailhead |> Set.length

let trailhead_scores_sum grid =
  find_trailheads grid |> List.sum (module Int) ~f:(trailhead_score grid)

let trailhead_rating_sum grid =
  find_trailheads grid |> List.sum (module Int) ~f:(trailhead_rating grid)
