open Core

let to_rules rule_list =
  List.map rule_list ~f:(fun s ->
      let split = String.split s ~on:'|' in
      match split with
      | [ l; r ] -> (Int.of_string l, Int.of_string r)
      | _ -> failwith "Error parsing rules")
  |> Int.Map.of_alist_multi
  |> Int.Map.map ~f:Int.Set.of_list

let parse_input s =
  let split_lines = String.split_lines s in
  let index_of_split =
    List.findi_exn split_lines ~f:(fun _i -> String.is_empty) |> fst
  in
  let rule_list = List.take split_lines index_of_split in
  let updates_string = List.drop split_lines (index_of_split + 1) in
  let updates =
    List.map updates_string ~f:(fun s ->
        String.split s ~on:',' |> List.map ~f:Int.of_string)
  in
  (to_rules rule_list, updates)

let is_valid_update rules update =
  let pages_in_update = Int.Set.of_list update in
  let indexes =
    List.mapi update ~f:(fun i n -> (n, i)) |> Int.Map.of_alist_multi
  in
  Map.for_alli indexes ~f:(fun ~key ~data:curr_page_indices ->
      let pages_that_should_come_after =
        Map.find rules key |> Option.value ~default:Int.Set.empty
      in
      let pages_to_check =
        Set.inter pages_in_update pages_that_should_come_after
      in
      let curr_page_max_index = List.last_exn curr_page_indices in
      Set.for_all pages_to_check ~f:(fun page ->
          let page_indices = Map.find_exn indexes page in
          let page_min_index = List.hd_exn page_indices in
          curr_page_max_index < page_min_index))

let valid_updates s =
  let rules, updates = parse_input s in
  List.filter updates ~f:(fun update -> is_valid_update rules update)

let non_valid_updates s =
  let rules, updates = parse_input s in
  List.filter updates ~f:(fun update -> not (is_valid_update rules update))

let middle_page update = List.nth_exn update (List.length update / 2)

let middle_pages_from_valid_updates s =
  valid_updates s |> List.map ~f:middle_page

let fix_update rules update =
  let add_page curr new_page =
    match curr with
    | [] -> [ new_page ]
    | curr ->
        let index =
          List.findi (List.rev curr) ~f:(fun _i page ->
              let page_set =
                Map.find rules page |> Option.value ~default:Int.Set.empty
              in
              Set.mem page_set new_page)
          |> Option.value_map ~f:fst ~default:(List.length curr)
        in
        let index = List.length curr - index in
        List.concat [ List.take curr index; [ new_page ]; List.drop curr index ]
  in
  List.fold update ~init:[] ~f:add_page

let middle_pages_from_invalid_updates_after_fixing s =
  let rules, _ = parse_input s in
  non_valid_updates s
  |> List.map ~f:(fix_update rules)
  |> List.map ~f:middle_page
