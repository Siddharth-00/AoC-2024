open Core

let rec diff_list = function
  | x :: x' :: xs -> (x' - x) :: diff_list (x' :: xs)
  | _ -> []

let reports_safe =
  List.map ~f:(fun xs ->
      let ds = diff_list xs in
      let all_positive = List.for_all ds ~f:(fun n -> n > 0) in
      let all_negative = List.for_all ds ~f:(fun n -> n < 0) in
      let diff_in_range =
        List.for_all ds ~f:(fun n -> 1 <= abs n && abs n <= 3)
      in
      (all_positive || all_negative) && diff_in_range)

let rec get_lists_minus_one' xs ys =
  match (xs, ys) with
  | xs, y :: ys ->
      List.append xs ys :: get_lists_minus_one' (List.append xs [ y ]) ys
  | _, [] -> []

let get_lists_minus_one = get_lists_minus_one' []

let reports_safe_with_dampener =
  List.map ~f:(fun xs ->
      let xxs_minus_one = get_lists_minus_one xs in
      List.exists xxs_minus_one ~f:(fun xs_minus_one ->
          let ds = diff_list xs_minus_one in
          let all_positive = List.for_all ds ~f:(fun n -> n > 0) in
          let all_negative = List.for_all ds ~f:(fun n -> n < 0) in
          let diff_in_range =
            List.for_all ds ~f:(fun n -> 1 <= abs n && abs n <= 3)
          in
          (all_positive || all_negative) && diff_in_range))
