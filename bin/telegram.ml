(*
 TODO: implement in terms of first class module
*)
open Cohttp_lwt_unix

let html_markup_message logo body =
    Printf.sprintf "<b>%s</b> <i>%s</i>" logo body

let post_message ~config ~message =
  let basic_headers = Cohttp.Header.init_with "Content-type" "application/json"
    and (json : Yojson.Basic.t)  = `Assoc [
      ("text", `String (message));
      ("chat_id", `Int config.Config.chat_id);
      ("disable_web_page_preview", `Bool true);
      ("parse_mode", `String "HTML");
    ]
    and send_message_uri = Printf.sprintf "%s/sendMessage" config.Config.endpoint
    in
      let body =
        json
        |> Yojson.Basic.to_string
        |> Cohttp_lwt.Body.of_string
      in
      Lwt.Infix.(
        (* TODO: propagate error to caller, at least log errors. *)
        (* TODO: log rate limiter **)
        Client.post (Uri.of_string send_message_uri) ~headers:basic_headers ~body:body
        >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
        >|= fun _ -> ()
      )
