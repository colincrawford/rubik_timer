open Core

type t = {start: Time.t; stop: Time.t}

let start_timer () : t =
  let now = Time.now () in
  let start = now and stop = now in
  {start; stop}

let%test _ =
  let timer = start_timer () in
  timer.start = timer.stop

let stop_timer (timer : t) : t =
  let stop = Time.now () in
  {timer with stop}

let elapsed_time (timer : t) : string =
  let diff = Time.diff timer.stop timer.start in
  Time.Span.to_string_hum diff
