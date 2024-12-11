open Core

let blink =
  Utils.memoize2_recursive
    (module Int)
    (module Int)
    (fun recurse x n ->
      if n = 0 then 1
      else if x = 0 then recurse 1 (n - 1)
      else
        let num_digits =
          Float.(log10 (of_int x)) |> Int.of_float |> Int.( + ) 1
        in
        if num_digits % 2 = 0 then
          let first_half, second_half =
            Int.to_string x |> String.to_list
            |> (Fn.flip List.split_n) (num_digits / 2)
          in
          recurse (Int.of_string (String.of_list first_half)) (n - 1)
          + recurse (Int.of_string (String.of_list second_half)) (n - 1)
        else recurse (x * 2024) (n - 1))
