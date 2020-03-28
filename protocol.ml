open Lwt.Infix

module Commands (S : Mirage_stack.V4) = struct
  module Stack = struct
    type t = {
        instance  : S.TCPV4.t;
        node_ip   : Ipaddr.V4.t;
        node_port : int
      }

    let connect stack_info =
      match stack_info with { instance; node_ip; node_port; } ->
        S.TCPV4.create_connection instance (node_ip, node_port)

    let disconnect stack_info =
      S.TCPV4.disconnect stack_info.instance
  end

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
                    Logs.err (fun f -> f "response receiving error: %a" pp_error err); Lwt.return_unit
                 | Ok response -> (
                   match response with
                   | `Data payload -> Logs.debug (fun f -> f "node responded with payload:\n%s" (Cstruct.to_string payload))
                   | `Eof -> Logs.debug (fun f -> f "EOF")
                 );
                 Lwt.return_unit

  let state_ping flow =
    request_state flow
end
