open Core

module Space_with_index = struct
  module T = struct
    type t = { space : int; starting_index : int } [@@deriving sexp]

    let compare t t' =
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
        (t.starting_index, t.space)
        (t'.starting_index, t'.space)
  end

  include T
  include Comparable.Make (T)
end

let expand_blocks =
  List.concat_mapi ~f:(fun i b ->
      if b < 0 then print_endline (sprintf "i=%d, b=%d" i b);
      if i % 2 = 0 then List.init b ~f:(fun _ -> i / 2)
      else List.init b ~f:(fun _ -> -1))

let to_free_used_sets blocks =
  let _, free_space, used_space =
    List.foldi blocks ~init:(0, [], [])
      ~f:(fun k (i, free_space, used_space) b ->
        if k % 2 = 0 then
          let id = k / 2 in
          (i + b, free_space, (b, id, i) :: used_space)
        else
          ( i + b,
            { Space_with_index.space = b; starting_index = i } :: free_space,
            used_space ))
  in
  (List.sort free_space ~compare:Space_with_index.compare, used_space)

let compress_filesystem ~expanded_blocks =
  let used_blocks = List.filter expanded_blocks ~f:Int.is_non_negative in
  let total_used_blocks = List.length used_blocks in
  let used_blocks_rev = List.rev used_blocks in
  List.fold_mapi expanded_blocks ~init:used_blocks_rev
    ~f:(fun i file_blocks id ->
      if i >= total_used_blocks then (file_blocks, -1)
      else if id >= 0 then (file_blocks, id)
      else
        match file_blocks with
        | b :: bs -> (bs, b)
        | [] -> failwith "Not enough used blocks")
  |> snd

let checksum ~expanded_blocks =
  List.mapi expanded_blocks ~f:Int.( * ) |> List.sum (module Int) ~f:Fn.id

let compress_and_get_checksum ~expanded_blocks =
  let compressed = compress_filesystem ~expanded_blocks in

  checksum ~expanded_blocks:(List.filter compressed ~f:Int.is_non_negative)

(* id * i + id * (i + 1) + id * (i + 2) + ... + id * (i + a)  *)
let calc_contiguous_checksum id a i = ((a * (a - 1) / 2) + (a * i)) * id

let compress_continuous_and_get_checksum blocks =
  let free_space, used_space = to_free_used_sets blocks in
  List.fold used_space ~init:(0, free_space)
    ~f:(fun (curr_total, rem_free_space) (b, id, i) ->
      match
        List.find rem_free_space
          ~f:(fun { Space_with_index.space; starting_index } ->
            i > starting_index && space >= b)
      with
      | None -> (curr_total + calc_contiguous_checksum id b i, rem_free_space)
      | Some ({ Space_with_index.space; starting_index } as free_spot) ->
          if starting_index >= i then
            (curr_total + calc_contiguous_checksum id b i, rem_free_space)
          else
            let new_free_space =
              {
                Space_with_index.space = space - b;
                starting_index = starting_index + b;
              }
            in
            let new_free_list =
              List.filter rem_free_space
                ~f:(Fn.non (Space_with_index.equal free_spot))
            in
            let new_free_list =
              List.sort ~compare:Space_with_index.compare
                (new_free_space :: new_free_list)
            in
            ( curr_total + calc_contiguous_checksum id b starting_index,
              new_free_list ))
  |> fst
