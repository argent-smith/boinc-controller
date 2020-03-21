open Lwt.Infix

module BoincController (Time : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module C = Protocol.Commands(S)

  let node_ip = Ipaddr.V4.make 192 168 47 51
  and node_port = 31416

  let start _time stack =
    let _ = C.stack ~instance:(Some (S.tcpv4 stack)) () in
    let rec loop () =
      let duration = 1 in
      C.state_ping ~node_ip ~node_port
      >>= fun () -> Time.sleep_ns (Duration.of_sec duration)
      >>= fun () -> loop ()
    in
    Lwt.join [loop ()]
end
