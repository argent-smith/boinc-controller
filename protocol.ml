open Lwt
open Lwt.Infix

module Commands (S : Mirage_stack.V4) = struct
  module TCP = S.TCPV4
  module IP = Ipaddr.V4

  module StateRequest = struct
    let query = "<get_state/>"

    let query_wrapper =
      Fmt.str "<boinc_gui_request>%s</boinc_gui_request>\x03"

    let to_payload = Cstruct.of_string

    let watch_write_or closer = function
      | Error err ->
         Logs.err (fun f -> f "command transmission error: %a" TCP.pp_write_error err);
         closer ()
      | _ -> return_unit

    let get_result_or closer = function
      | Error err ->
         Logs.err (fun f -> f "response receiving error: %a" TCP.pp_error err);
         closer ()
         >>= (fun () -> return @@ Error ())
      | Ok response ->
         match response with
         | `Data payload ->
            let str = Cstruct.to_string payload in
            Logs.debug (fun f -> f "node responded with payload:\n%s" str);
            return @@ Ok str
         | `Eof -> Logs.debug (fun f -> f "EOF"); return @@ Error ()

    let send_to flow =
      let open TCP in
      let closer = (fun () -> close flow)
      and reader = (fun () -> read flow)
      and writer = write flow in
      query_wrapper query
      |> to_payload
      |> writer
      >>= watch_write_or closer
      >>= reader
      >>= get_result_or closer
  end

  let state_ping flow =
    let addr, port = TCP.dst flow in
    Logs.debug (fun f -> f "pinging %s:%d" (IP.to_string addr) port);
    StateRequest.send_to flow
    >>= fun _ -> return_unit
end
