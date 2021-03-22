open Core
open Rubiks_timer

type t = { time: string; }

let of_timer (timer: Timer.t): t = { time = Timer.elapsed_time timer }

let to_json (score: t): string =
  let time_str = score.time in
  "{\"time\":\"" ^ time_str ^ "}"

let from_json (s: string): t option =
  let parsed = Yojson.Basic.from_string s in
  match parsed with
  | `String s -> Some { time = s }
  | _ -> None

let list_to_json (scores: t list) =
  let serialized_scores = List.map ~f:to_json scores in
  let list_json = List.
