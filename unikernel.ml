open Lwt.Infix

module BoincController (Time : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module C = Protocol.Commands(S)

  let node_ip = Ipaddr.V4.make 192 168 47 51
  and node_port = 31416

  let start _time stack =
    let open C.Stack in
    let stack_option = Some {
                           instance = S.tcpv4 stack;
                           node_ip;
                           node_port
                         } in
    let _ = C.stack ~stack_option () in
    let rec loop () =
      let duration = 1 in
      C.state_ping ()
      >>= fun () -> Time.sleep_ns (Duration.of_sec duration)
      >>= fun () -> loop ()
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
