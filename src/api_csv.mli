


type t

val load_from_file : file:string -> t
  
val fold_left : ('a -> string list -> 'a) -> 'a -> t -> 'a

val of_string : string -> t

val row_of_string : string -> string list
  
val row_of_string_opt : string -> string list option
