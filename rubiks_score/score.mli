open Rubiks_timer

type t

val of_timer: Timer.t -> t

val to_json: t -> string

val list_to_json: t list -> string

val from_json: string -> t option
