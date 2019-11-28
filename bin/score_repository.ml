open Core
open Async

type t = { file_name : string }

let create ?(file_name="rubiks_scores.txt") () = { file_name }

let save_score repo score =
  let serialized_score = Score.to_string score in
  Writer.with_file
    ~append:true
    repo.file_name
    ~f:(fun writer -> return (Writer.write_line writer serialized_score))
  >>| (const score)

let get_scores repo =
  Async.Sys.file_exists repo.file_name >>= function
  | `Yes -> begin 
    Reader.file_contents repo.file_name
    >>| (fun file_content -> String.split ~on:'\n' file_content)
    >>| (fun serialized_scores -> List.map ~f:Score.of_string serialized_scores)
  end
  | _ -> return []



