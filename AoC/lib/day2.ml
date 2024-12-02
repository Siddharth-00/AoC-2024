open Core

let rec all_increasing_and_diff_in_range = function
  | x :: x' :: xs ->
      (x' > x && x' - x >= 1 && x' - x <= 3)
      && all_increasing_and_diff_in_range (x' :: xs)
  | [ _ ] | [] -> true

let reports_safe =
  List.map ~f:(fun xs ->
      let xs_rev = List.rev xs in
      all_increasing_and_diff_in_range xs
      || all_increasing_and_diff_in_range xs_rev)

let rec all_increasing_and_diff_in_range_with_one_error = function
  | x :: x' :: xs ->
      (x' > x && x' - x >= 1 && x' - x <= 3)
      && all_increasing_and_diff_in_range_with_one_error (x' :: xs)
      || all_increasing_and_diff_in_range (x' :: xs)
  | [ _ ] | [] -> true

let reports_safe_with_dampener =
  List.map ~f:(fun xs ->
      let xs_rev = List.rev xs in
      all_increasing_and_diff_in_range_with_one_error xs
      || all_increasing_and_diff_in_range_with_one_error xs_rev)
