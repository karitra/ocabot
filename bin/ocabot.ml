open Core

let proc c urls =
  Lwt_main.run (Pinger.ping ~config:c ~urls)

let () =
  Command.run ~version:"0.1" ~build_info:"OcaBot:linux" (Config.command proc)
