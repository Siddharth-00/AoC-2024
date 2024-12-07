open Core

module Operations = struct
  type t = Add | Mul | Concatenate [@@deriving enumerate]

  let to_op = function
    | Add -> Int.( + )
    | Mul -> Int.( * )
    | Concatenate -> fun x y -> Int.of_string (Int.to_string x ^ Int.to_string y)
end

let parse_input input =
  let regex = Re.Pcre.regexp "^(\\d+): (.+)$" in
  List.map input ~f:(fun s ->
      let match_ = Re.exec regex s in
      let test_value = Re.Group.get match_ 1 |> Int.of_string in
      let numbers =
        Re.Group.get match_ 2 |> String.split ~on:' '
        |> List.map ~f:Int.of_string
      in
      (test_value, numbers))
  |> Int.Map.of_alist_exn

let rec is_valid_test_value test_value = function
  | [] -> false
  | [ n ] -> Int.equal n test_value
  | n :: n' :: ns ->
      List.exists Operations.all ~f:(fun op ->
          is_valid_test_value test_value ((Operations.to_op op) n n' :: ns))

let valid_test_values =
  Map.filteri ~f:(fun ~key:test_value ~data:numbers ->
      is_valid_test_value test_value numbers)
