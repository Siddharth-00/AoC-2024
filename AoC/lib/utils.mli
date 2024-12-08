open Core

module Position : sig
  type t = int * int [@@deriving compare, sexp]

  include Comparable.S with type t := t

  val add : t -> t -> t
  val dist : t -> t -> t
end

val read_file_as_string : string -> string
val read_file_lines : string -> string list
val read_file_as_ints : string -> int list
val read_file_as_split_ints : string -> int list * int list
val read_file_as_2d_int_matrix : string -> int list list
val read_file_as_2d_char_matrix : string -> char list list
val print_2d_matrix : 'a list list -> f:('a -> string) -> unit
val permutations : 'a list -> 'a list list
