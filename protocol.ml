open Lwt.Infix

module Commands (S : Mirage_stack.V4) = struct
  module Stack = struct
    type t = {
        instance  : S.TCPV4.t;
        node_ip   : Ipaddr.V4.t;
        node_port : int
      }
  end

  let stack_instance : Stack.t option ref = ref None

  let stack ?(stack_option = None) () =
    let opt =
      match stack_option with
      | None -> !stack_instance
      | Some instance -> stack_instance := Some instance; !stack_instance
    in
    Option.get opt

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

  let connect () =
    let open Stack in
    match stack () with { instance; node_ip; node_port; } ->
      Logs.debug (fun f -> f "pinging %a:%i"  Ipaddr.V4.pp node_ip node_port);
      S.TCPV4.create_connection instance (node_ip, node_port)

  let disconnect () =
    S.TCPV4.disconnect Stack.((stack ()).instance)

  let state_ping () =
    connect ()
    >>= function
    | Error err -> Logs.err (fun f -> f "connection error: %a" S.TCPV4.pp_error err); Lwt.return_unit
    | Ok flow -> request_state flow
       >>= disconnect
end
