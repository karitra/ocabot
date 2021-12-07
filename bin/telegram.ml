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


(* TODO: WIP *)
let get_command_updates ~config ~urls =
  let compose_udpate_uri endpoint timeout offset =
    let get_update_uri = Printf.sprintf "%s/getUpdate" endpoint
    in
    let url = Uri.of_string get_update_uri
    in
      Uri.add_query_params' url [
        ("timeout", Int.to_string timeout);
        ("offset", Int.to_string offset)
      ]
  in
  let request_uri off = compose_udpate_uri
    config.Config.endpoint
    config.Config.update_check_interval_seconds
    off
  in
  let rec subscribe offset =
    Lwt.Infix.(
      Client.get (request_uri offset)
      >>= fun (_, body) -> Cohttp_lwt.Body.drain_body body
      >>= (fun _ -> Lwt_io.printl "done")
      >>= (fun () -> post_message ~config ~message:(String.concat "," urls))
      >>= (fun () -> subscribe (offset + 1))
    )
  in
    subscribe 0
