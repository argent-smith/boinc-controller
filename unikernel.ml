open Lwt.Infix

module BoincController (Time : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module TCP = S.TCPV4
  module C = Protocol.Commands(S)

  let node_ip = Ipaddr.V4.make 192 168 47 51
  and node_port = 31416

  let start _time stack =
    let rec loop () =
      let tcp = S.tcpv4 stack
      and duration = 1 in
      TCP.create_connection tcp (node_ip, node_port)
      >>= (fun conn_result ->
        match conn_result with
        | Error err ->
           Logs.err (fun f -> f "connection error: %a" TCP.pp_error err);
           TCP.disconnect tcp
        | Ok flow ->
           C.state_ping flow
           >>= fun () -> TCP.close flow
      )
      >>= fun () -> Time.sleep_ns (Duration.of_sec duration)
      >>= fun () -> loop ()
    in
    Lwt.join [loop ()]
end

(*

module dependency plan

BoincController -> Commands
                -> CommandA
                -> ...       -> functor Command (CD : CommandData) -> module type CommandData
                -> CommandZ

*)
