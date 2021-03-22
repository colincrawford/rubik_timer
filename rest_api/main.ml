open Core
open Async
open Httpaf
open Httpaf_async
open Dispatch

open Rubiks_score
open Rubiks_score_repository

let score_repo = Score_repository.create ()

let get_scores_handler _keys _rest reqd =
  let scores = Score_repository.get_scores score_repo in
  let scores_json = Score.l
  let headers = Headers.of_list ["connection", "close"] in
  let response = Response.create ~headers `OK in
  Reqd.respond_with_string reqd response "Hi there!\n"


let home_handler _keys _rest reqd =
  let headers = Headers.of_list ["connection", "close"] in
  let response = Response.create ~headers `OK in
  Reqd.respond_with_string reqd response "Hi there!\n"

let not_found_handler path reqd =
  let headers = Headers.of_list ["connection", "close"] in
  let response = Response.create ~headers `OK in
  let body = Printf.sprintf "\"%s\" not found\n" path in
  Reqd.respond_with_string reqd response body


let request_handler _inet_socket reqd =
  let path = reqd |> Reqd.request |> fun req -> req.target in
  let () = Stdio.printf "%s\n" path; Out_channel.flush Stdio.stdout in
  let routes = [
    "/", home_handler
  ] in
  match DSL.dispatch routes path with
  | Result.Ok handler -> handler reqd
  | Result.Error _err -> not_found_handler path reqd

let error_handler _ ?request:_ error start_response =
  let response_body = start_response Headers.empty in
   begin match error with
    | `Exn exn ->
      Body.write_string response_body (Exn.to_string exn);
      Body.write_string response_body "\n";
    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error)
    end;
    Body.close_writer response_body
  
let create_server port =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  let connection_handler = Server.create_connection_handler
      ~request_handler
      ~error_handler
  in
  Tcp.Server.create_sock
    ~on_handler_error:`Raise
    where_to_listen
    connection_handler
  >>= fun _server ->
  Stdio.printf "Listening on port %i\n" port;
  Out_channel.flush Stdio.stdout;
  Deferred.never ()

let main () = create_server 5002

let command =
  Command.async
    ~summary:"REST API"
    ~readme:(fun () -> "REST API")
    (Command.Param.return main)

let () = Command.run ~version:"1.0" command
