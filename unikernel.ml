open Lwt.Infix

module BoincController (Time : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module C = Protocol.Commands(S)
  module St = C.Stack

  let node_ip = Ipaddr.V4.make 192 168 47 51
  and node_port = 31416

  let start _time stack =
    let stack_info = {
        St.instance = S.tcpv4 stack;
        St.node_ip;
        St.node_port
      } in
    let rec loop () =
      St.connect stack_info
      >>= function
      | Error err -> Logs.err (fun f -> f "connection error: %a" S.TCPV4.pp_error err); Lwt.return_unit
      | Ok flow ->
         let duration = 1 in
         C.state_ping flow
         >>= fun () -> Time.sleep_ns (Duration.of_sec duration)
         >>= fun () -> loop ()
         >>= fun () -> S.TCPV4.close flow
         >>= fun () -> St.disconnect stack_info
    in
    Lwt.join [loop ()]
end

(*

module dependency plan

BoincController -> Stack [type stack_info; val connect; val disconnect
                -> Commands
                -> CommandA
                -> ...       -> functor Command (CD : CommandData) -> module type CommandData
                -> CommandZ

*)
