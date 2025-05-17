let rec loop env =
  print_string "mallee> ";
  flush stdout;
  match In_channel.input_line stdin with
  | None -> print_newline ()
  | Some line -> (
      match String.trim line with
      | "" -> loop env
      | ":quit" | ":q" -> ()
      | src -> (
          match Mallee.Check.run_line env src with
          | Ok (env', output) ->
              print_endline output;
              loop env'
          | Error message ->
              print_endline message;
              loop env))

let () =
  print_endline "mallee: type inference REPL (:quit to leave)";
  loop Mallee.Check.initial_env
