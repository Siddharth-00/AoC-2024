open Core

module Direction = struct
  type t = Up | Down | Left | Right

  let to_char = function Up -> '^' | Down -> 'v' | Left -> '<' | Right -> '>'

  let of_char = function
    | '^' -> Some Up
    | '>' -> Some Right
    | '<' -> Some Left
    | 'v' -> Some Down
    | _ -> None

  let to_index_change = function
    | Up -> (-1, 0)
    | Right -> (0, 1)
    | Down -> (1, 0)
    | Left -> (0, -1)

  let rotate_90 = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

  let to_int = function Up -> 0 | Right -> 1 | Down -> 2 | Left -> 3
  let compare x y = Int.compare (to_int x) (to_int y)

  let sexp_of_t = function
    | Up -> Sexp.of_string "Up"
    | Down -> Sexp.of_string "Down"
    | Right -> Sexp.of_string "Right"
    | Left -> Sexp.of_string "Left"

  let t_of_sexp = function
    | Sexp.Atom "Up" -> Up
    | Sexp.Atom "Right" -> Right
    | Sexp.Atom "Down" -> Down
    | Sexp.Atom "Left" -> Left
    | _ -> failwith "Error parsing direction to sexp"
end

module Grid = struct
  type t = {
    grid : char array array;
    current_position : int * int;
    current_direction : Direction.t;
  }

  let grid_size { grid; _ } = (Array.length grid, Array.length grid.(0))

  let position_out_of_bounds t (i, j) =
    let m, n = grid_size t in
    not (0 <= i && i < m && 0 <= j && j < n)

  let iter ({ grid; current_position = i, j; current_direction } as t) =
    let i', j' =
      let di, dj = Direction.to_index_change current_direction in
      (i + di, j + dj)
    in
    match position_out_of_bounds t (i', j') with
    | true -> None
    | false -> (
        match grid.(i').(j') with
        | '#' ->
            Some
              {
                grid;
                current_position = (i, j);
                current_direction = Direction.rotate_90 current_direction;
              }
        | '.' | '^' | '>' | '<' | 'v' ->
            Some { grid; current_position = (i', j'); current_direction }
        | _ -> failwith "Error parsing grid")
end

let is_starting_position c = Direction.of_char c |> Option.is_some

let starting_position grid =
  Array.find_mapi_exn grid ~f:(fun i row ->
      Array.findi row ~f:(fun _j -> is_starting_position)
      |> Option.map ~f:(Tuple2.create i))

module Position = struct
  module T = struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
  end

  include T
  include Comparable.Make (T)
end

module Position_with_dir = struct
  module T = struct
    type t = Position.t * Direction.t

    let create position direction = (position, direction)
    let compare = Tuple2.compare ~cmp1:Position.compare ~cmp2:Direction.compare
    let t_of_sexp = Tuple2.t_of_sexp Position.t_of_sexp Direction.t_of_sexp
    let sexp_of_t = Tuple2.sexp_of_t Position.sexp_of_t Direction.sexp_of_t
  end

  include T
  include Comparable.Make (T)
end

let rec iter_and_record_position curr_grid visited =
  match Grid.iter curr_grid with
  | None -> `Out_of_bounds visited
  | Some ({ grid = _; current_position; current_direction } as new_grid) ->
      let new_position_with_dir =
        Position_with_dir.create current_position current_direction
      in
      if Set.mem visited new_position_with_dir then `Cycle visited
      else
        let new_visited = Set.add visited new_position_with_dir in
        iter_and_record_position new_grid new_visited

let distinct_positions grid =
  let i, (j, dir_char) = starting_position grid in
  let dir = Direction.of_char dir_char |> Option.value_exn in
  let grid_t =
    { Grid.grid; current_position = (i, j); current_direction = dir }
  in

  match
    iter_and_record_position grid_t
      (Position_with_dir.Set.singleton (Position_with_dir.create (i, j) dir))
  with
  | `Out_of_bounds visited_with_dir | `Cycle visited_with_dir ->
      Position.Set.map visited_with_dir ~f:fst

let num_distinct_positions grid = distinct_positions grid |> Set.length

let add_blockage { Grid.grid; current_position = _; current_direction = _ }
    (i, j) =
  Array.set grid.(i) j '#'

let remove_blockage { Grid.grid; current_position = _; current_direction = _ }
    (i, j) =
  Array.set grid.(i) j '.'

let num_blockages_with_cycle grid =
  let i, (j, dir_char) = starting_position grid in
  let dir = Direction.of_char dir_char |> Option.value_exn in
  let grid_t =
    { Grid.grid; current_position = (i, j); current_direction = dir }
  in
  let distinct_positions = distinct_positions grid in
  let distinct_positions =
    Set.remove distinct_positions ((i, j) : Position.t)
  in
  Set.count distinct_positions ~f:(fun p ->
      add_blockage grid_t p;
      match
        iter_and_record_position grid_t
          (Position_with_dir.Set.singleton
             (Position_with_dir.create (i, j) dir))
      with
      | `Out_of_bounds _ ->
          remove_blockage grid_t p;
          false
      | `Cycle _ ->
          remove_blockage grid_t p;
          true)
