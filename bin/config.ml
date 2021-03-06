open Core

type telegram_config = {
  chat_id: int;
  endpoint: string;
  update_check_interval_seconds: int;
}

type bot_config = {
  telegram_config: telegram_config;
  check_timeout_seconds: int;
  poll_interval: int;
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
        and check_timeout_seconds = flag "--check-timeout" (optional_with_default 15 int) ~doc:"url check timeout"
        and update_check_interval_seconds = flag "--update-check-timeout" (optional_with_default 10 int) ~doc:"telegram update check interval"
        and poll_interval = flag "--poll-interval" (optional_with_default 15 int) ~doc:"urls poll interval"
        and parallel_ping = flag "--parallel-ping" no_arg ~doc:"run pinger in parralel mode"
      in
      let telegram_config = {
        chat_id;
        endpoint;
        update_check_interval_seconds
      }
      in
      let cfg = {
        telegram_config;
        check_timeout_seconds;
        poll_interval;
        parallel_ping
      }
      in
      fun () ->
        exec cfg urls
    )
