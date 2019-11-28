open Core
open Async
open Rubik_timer

let stdin = Lazy.force Reader.stdin
let stdout = Lazy.force Writer.stdout

let log_to_stdout s =
  Writer.write stdout s;
  Writer.flushed stdout

let get_user_input () = Reader.read_line stdin

let rec run_timer_loop score_repo =
  (log_to_stdout "Ready? ") >>=
  get_user_input >>= (fun line ->
      let timer = Timer.start_timer () in
      match line with
      | `Eof -> log_to_stdout "EOF!"
      | `Ok _ -> begin
          (get_user_input ()) >>= (fun _ ->
          let timer = Timer.stop_timer timer in
          let score = Score.of_timer timer in
          (Score_repository.save_score score_repo score) >>= (fun score ->
              (Score.to_string score)
              |> (fun score -> log_to_stdout (score ^ "\n"))) >>= (fun _ ->
              run_timer_loop score_repo))
        end)

let create_intro_msg previous_scores =
  let previous_scores_str =
    (List.map ~f:(fun score -> (Score.to_string score) ^ "\n") previous_scores)
    |> List.fold ~init:"" ~f:(^) in
  let intro_message =
    "==========\n" ^
    "Your Previous Times:\n" ^
    previous_scores_str ^
    "==========\n" in
  intro_message


let main () =
  let score_repo = Score_repository.create () in
  Score_repository.get_scores score_repo >>= (fun previous_scores ->
      let intro_message = create_intro_msg previous_scores in
      (log_to_stdout intro_message) >>= (fun _ ->
          run_timer_loop score_repo))

let command = Command.async
    ~summary:"CLI timer"
    ~readme:(fun () -> "CLI timer")
    (Command.Param.return main)

let () = Command.run ~version:"1.0" command
