open Lwt.Infix

module Commands (S : Mirage_stack.V4) = struct
  module TCP = S.TCPV4
  module IP = Ipaddr.V4

  let request_state flow =
    let open S.TCPV4 in
    let request_text =
      "<boinc_gui_request>\
       <get_state/>\
       </boinc_gui_request>\x03" in
    let payload = Cstruct.of_string request_text in
    write flow payload
    >>= (
      function
      | Error err ->
         Logs.err (fun f -> f "command transmission error: %a" pp_write_error err); Lwt.return_unit
      | _ -> Lwt.return_unit
    )
    >>= fun () -> read flow
    >>= function | Error err ->
                    Logs.err (fun f -> f "response receiving error: %a" pp_error err);
                    TCP.close flow
                 | Ok response -> (
                   match response with
                   | `Data payload -> Logs.debug (fun f -> f "node responded with payload:\n%s" (Cstruct.to_string payload))
                   | `Eof -> Logs.debug (fun f -> f "EOF")
                 );
                 Lwt.return_unit

  let state_ping flow =
    let addr, port = TCP.dst flow in
    Logs.debug (fun f -> f "pinging %s:%d" (IP.to_string addr) port);
    request_state flow
end
