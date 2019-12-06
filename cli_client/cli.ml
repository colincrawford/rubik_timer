open Core
open Async
open Rubiks_timer
open Rubiks_score
open Rubiks_score_repository

let stdin = Lazy.force Reader.stdin

let stdout = Lazy.force Writer.stdout

let log_to_stdout s =
  Writer.write stdout s;
  Writer.flushed stdout

let get_user_input () = Reader.read_line stdin

let rec run_timer_loop score_repo =
  let%bind () = log_to_stdout "Ready? " in
  let%bind line = get_user_input () in
  let timer = Timer.start_timer () in
  match line with
  | `Eof -> log_to_stdout "EOF!"
  | `Ok _ ->
      let%bind _ = get_user_input () in
      let timer = Timer.stop_timer timer in
      let score = Score.of_timer timer in
      let%bind score = Score_repository.save_score score_repo score in
      let%bind () = log_to_stdout (Score.to_string score ^ "\n") in
      run_timer_loop score_repo

let create_intro_msg previous_scores =
  let previous_scores_str =
    List.map ~f:(fun score -> Score.to_string score ^ "\n") previous_scores
    |> List.fold ~init:"" ~f:( ^ )
  in
  let intro_message =
    "==========\n" ^ "Your Previous Times:\n" ^ previous_scores_str
    ^ "==========\n"
  in
  intro_message

let main () =
  let score_repo = Score_repository.create () in
  let%bind previous_scores = Score_repository.get_scores score_repo in
  let intro_message = create_intro_msg previous_scores in
  let%bind () = log_to_stdout intro_message in
  run_timer_loop score_repo

let command =
  Command.async ~summary:"CLI timer"
    ~readme:(fun () -> "CLI timer")
    (Command.Param.return main)

let () = Command.run ~version:"1.0" command
