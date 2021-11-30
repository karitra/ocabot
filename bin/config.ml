open Core

type telegram_config = {
  chat_id: int;
  endpoint: string;
}

type bot_config = {
  telegram_config: telegram_config;
  check_timeout_seconds: int;
  parallel_ping: bool;
}

let command exec =
  Command.basic
    ~summary: "Tiny pinger bot"
    Command.Let_syntax.(
      let%map_open
        urls = anon (non_empty_sequence_as_list ("urls" %: string))
        and endpoint = flag "--telegram-url" (required string) ~doc:"telegram bot url"
        and chat_id = flag "--telegram-chat" (required int) ~doc:"telegram chat id"
        and check_timeout_seconds = flag "--check-timeout" (required int) ~doc:"single url check timeout"
        and parallel_ping = flag "--parallel-ping" no_arg ~doc:"run pinger in parralel mode"
      in
      let telegram_config = {chat_id; endpoint} in
      let cfg = {telegram_config; check_timeout_seconds; parallel_ping} in
      fun () ->
        exec cfg urls
    )
