open Core


let () =
let proc config urls =
    let telegram_config = config.Config.telegram_config in
    let sched_update_listener () = Telegram.get_command_updates ~config:telegram_config ~urls
    in
    Lwt.async sched_update_listener;
    Lwt_main.run (Pinger.ping ~config ~urls)
  in
  Command.run ~version:"0.1" ~build_info:"OcaBot:linux" (Config.command proc)
