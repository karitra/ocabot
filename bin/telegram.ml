open Cohttp_lwt_unix

let post_message ~config ~message =
  let basic_headers = Cohttp.Header.init_with "Content-type" "application/json"
  in
    message
    |> Cohttp_lwt.Body.of_string
    |> Client.post (Url.from_string config.Config.endpoint) ?headers:basic_headers
