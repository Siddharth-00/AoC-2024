open Core

let pattern = "XMAS"

let find_xmas_horizontal grid =
  List.sum
    (module Int)
    grid
    ~f:(fun s ->
      String.of_list s
      |> String.substr_index_all ~may_overlap:false ~pattern
      |> List.length)

let get_diagonal_matrix grid =
  let m = List.length grid in
  let n = List.length (List.hd_exn grid) in
  let diagonal_matrix = List.init (m + n - 1) ~f:(fun _ -> []) in
  let add_row mat row i =
    let left_part = List.take mat (m - 1 - i) in
    let right_part = List.drop mat (m - 1 - i + n) in
    let middle_part = List.take (List.drop mat (m - 1 - i)) n in
    let middle_part' =
      List.zip_exn row middle_part |> List.map ~f:(fun (x, xs) -> x :: xs)
    in
    List.concat [ left_part; middle_part'; right_part ]
  in
  List.foldi grid ~init:diagonal_matrix ~f:(fun i mat row -> add_row mat row i)

let rotate_clockwise grid = List.transpose_exn grid |> List.map ~f:List.rev

let find_xmas grid =
  let rotate0 = grid in
  let rotate1 = rotate_clockwise rotate0 in
  let rotate2 = rotate_clockwise rotate1 in
  let rotate3 = rotate_clockwise rotate2 in
  let diag0 = get_diagonal_matrix rotate0 in
  let diag1 = get_diagonal_matrix rotate1 in
  let diag2 = get_diagonal_matrix rotate2 in
  let diag3 = get_diagonal_matrix rotate3 in
  let grids =
    [ rotate0; rotate1; rotate2; rotate3; diag0; diag1; diag2; diag3 ]
  in
  List.sum (module Int) grids ~f:find_xmas_horizontal

let find_x_mas' grid =
  let m, n = (List.length grid, List.length (List.hd_exn grid)) in
  let diag0 = get_diagonal_matrix grid in
  let diag1 = get_diagonal_matrix (rotate_clockwise grid) in
  let sam_indices0 =
    List.map diag0 ~f:(fun cs ->
        String.of_char_list cs
        |> String.substr_index_all ~may_overlap:false ~pattern:"SAM")
    |> List.concat_mapi ~f:(fun i indices ->
           List.map indices ~f:(fun j -> (i, j + 1)))
  in
  let sam_indices1 =
    List.map diag1 ~f:(fun cs ->
        String.of_char_list cs
        |> String.substr_index_all ~may_overlap:false ~pattern:"SAM")
    |> List.concat_mapi ~f:(fun i indices ->
           List.map indices ~f:(fun j -> (i, j + 1)))
  in
  let sam_indices0 =
    List.map sam_indices0 ~f:(fun (i, j) ->
        if i < n then (m - 1 - j, i - j)
        else (m - 1 - (i % (n - 1)) - j, n - 1 - j))
  in
  let sam_indices1 =
    List.map sam_indices1 ~f:(fun (i, j) ->
        if i < m then (m - 1 - i + j, n - 1 - j)
        else (j, n - 1 - (i % (m - 1)) - j))
  in

  let sam_indices_set0 = Utils.Position.Set.of_list sam_indices0 in
  let sam_indices_set1 = Utils.Position.Set.of_list sam_indices1 in
  Set.inter sam_indices_set0 sam_indices_set1 |> Set.length

let find_x_mas grid =
  let rotate0 = grid in
  let rotate1 = rotate_clockwise rotate0 in
  let rotate2 = rotate_clockwise rotate1 in
  let rotate3 = rotate_clockwise rotate2 in
  let grids = [ rotate0; rotate1; rotate2; rotate3 ] in
  List.sum (module Int) grids ~f:find_x_mas'
