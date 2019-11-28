open Rubik_timer

type t

val of_timer: Timer.t -> t

val to_string: t -> string

val of_string: string -> t
