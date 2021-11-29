open Cohttp_lwt_unix

let compute ~timeout_seconds ~f =
  let open Lwt.Infix in
  Lwt.pick [
      (f () >|= fun v -> `Completed v)
    ; (Lwt_unix.sleep timeout_seconds >|= fun () -> `Timeout)
  ]


let mark_timeout ~timeout_seconds ~url =
  Lwt.return (`FaliedByTimeoutStatus (url, timeout_seconds))


let select_by_status ~url ~body = function
 | 200 -> Lwt.return `OkStatus (* TODO: Lwt log *)
 | code ->
  Lwt.Infix.(
    body |> Cohttp_lwt.Body.to_string >|= fun b ->
      (* NOTE: here full body is read, but it seems that it is possilbe to send
         body to tg in async manner **)
      `NotOkStatus (url, code, b)
  )


let ping_url ~config ~url =
  Lwt.Infix.(
    let get () = Client.get (Uri.of_string url) in
    let timeout_seconds = Float.of_int config.Config.check_timeout_seconds
    in
    compute ~timeout_seconds ~f:get >>= function
    | `Timeout -> mark_timeout ~timeout_seconds:config.Config.check_timeout_seconds ~url
    | `Completed (resp, body) ->
        resp
        |> Response.status
        |> Cohttp.Code.code_of_status
        |> select_by_status ~url ~body
  )

let debug_result = function
  | `OkStatus -> Lwt.return_unit
  | `FaliedByTimeoutStatus (url, timeout_seconds) -> Lwt_io.printf "bad timeout, url: %s, timeout: %d\n" url timeout_seconds
  | `NotOkStatus (url, code, body) -> Lwt_io.printf "bad status, url: %s, http_code: %d, body: %s\n" url code body

let ping ~config ~urls =
  let filter = function
    | `OkStatus -> Lwt.return_false
    | `FaliedByTimeoutStatus _ | `NotOkStatus _ -> Lwt.return_true
  in
  Lwt.Infix.(
    Lwt_list.map_p (fun url -> ping_url ~config ~url ) urls
    >>= Lwt_list.filter_p filter
    >>= Lwt_list.iter_p debug_result
  )
