open Lwt.Infix

module BoincController (Time : Mirage_time.S) (S : Mirage_stack.V4) = struct
  let ping ~stack ~node_ip ~node_port =
    let open S.TCPV4 in
    Logs.info (fun f -> f "pinging %a:%i"  Ipaddr.V4.pp node_ip node_port);
    create_connection (S.tcpv4 stack) (node_ip, node_port)
    >>= function
    | Error err -> Logs.err (fun f -> f "connection error: %a" pp_error err); Lwt.return_unit
    | Ok flow ->
      let payload = Cstruct.of_string "<get_state/>\x03" in
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
                       | `Data payload -> Logs.info (fun f -> f "node responded with payload:\n%s" (Cstruct.to_string payload))
                       | `Eof -> Logs.info (fun f -> f "EOF")
                     );
                     Lwt.return_unit
      >>= fun () -> disconnect (S.tcpv4 stack)

  let node_ip = Ipaddr.V4.make 192 168 47 51
  and node_port = 31416

  let start _time stack =
    let rec loop () =
      let duration = 1 in
      ping ~stack ~node_ip ~node_port
      >>= fun () ->
        Time.sleep_ns (Duration.of_sec duration)
        >>= fun () -> loop ()
    in
    Lwt.join [loop ()]
end
