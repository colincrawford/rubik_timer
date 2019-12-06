open Rubiks_timer

type t = string

let of_timer (timer: Timer.t): t = Timer.elapsed_time timer

let to_string (score: t): string = score

let of_string (s: string): t = s
