open Core

let uncorrupt s =
  let regex = Re.Pcre.regexp "mul\\((\\d{1,3}),(\\d{1,3})\\)" in
  Re.matches regex s
  |> List.map ~f:(fun match_ ->
         let groups = Re.exec regex match_ in
         let x = Re.Group.get groups 1 |> Int.of_string in
         let y = Re.Group.get groups 2 |> Int.of_string in
         (x, y))

let uncorrupt_with_conditionals s =
  let regex =
    Re.Pcre.regexp "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)"
  in
  Re.matches regex s
  |> List.fold_map ~init:true ~f:(fun mul_enabled -> function
       | "do()" -> (true, None)
       | "don't()" -> (false, None)
       | match_string ->
           let groups = Re.exec regex match_string in
           let x = Re.Group.get groups 1 |> Int.of_string in
           let y = Re.Group.get groups 2 |> Int.of_string in
           if mul_enabled then (mul_enabled, Some (x, y))
           else (mul_enabled, None))
  |> snd |> List.filter_opt
