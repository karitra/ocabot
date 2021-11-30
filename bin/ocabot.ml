open Core


let () =
  let proc config urls =
    Lwt_main.run (Pinger.ping ~config ~urls)
  in
  Command.run ~version:"0.1" ~build_info:"OcaBot:linux" (Config.command proc)
