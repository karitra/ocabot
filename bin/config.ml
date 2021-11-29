open Core

(* let regular_url =
    Command.Arg_type.create (fun url ->
        match  with
        | `Yes -> filename
        | `No -> failwith "Not a regular file"
        | `Unknown -> failwith "Could not determine if this was a regular file") *)

type telegram_config = {
  chat_id: string;
  endpoint: string;
}

type bot_config = {
  telegram_config: telegram_config;
  check_timeout_seconds: int;
}

let command exec =
  Command.basic
    ~summary: "Tiny pinger bot"
    Command.Let_syntax.(
      let%map_open
        urls = anon (non_empty_sequence_as_list ("urls" %: string))
        and endpoint = flag "--telegram-url" (required string) ~doc:"telegram bot url"
        and chat_id = flag "--telegram-chat" (required string) ~doc:"telegram chat id"
        and check_timeout_seconds = flag "--check-timeout" (required int) ~doc:"single url check timeout"
      in
      let telegram_config = {chat_id; endpoint} in
      let cfg = {telegram_config; check_timeout_seconds} in
      fun () ->
        exec cfg urls
    )
