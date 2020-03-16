open Mirage

let main =
  let packages = [
      package "duration"
    ] in
  foreign ~packages
    "Unikernel.BoincController" (time @-> stackv4 @-> job)

let () =
  let stack = generic_stackv4 default_network in
  register "boinc_controller" [main $ default_time $ stack]
