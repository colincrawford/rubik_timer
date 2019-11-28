open Async

type t

val create: ?file_name: string -> unit -> t

val save_score: t -> Score.t -> Score.t Deferred.t

val get_scores: t -> Score.t list Deferred.t
