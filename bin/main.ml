open Lwt
open Lwt.Infix

(*BKMRK/TODO: Handle exceptions from malicious clients *)
(*  BKMRK/TODO: test badly implemented client sending Json with the wrong field names and/or types. I think it will take the server down.*)
(*BKMRK/TODO: make multiclient by being able to vary the port (I think) *)
(*BKMRK/TODO: get client and server to pass in name to put in prompt for eventual multi-client support *)
(*BKMRK/TODO: implement hostname resolution *)
(*BKMRK/FIXME: problem where sometimes the old server is running on the old port (could fix with port variation) *)

(** Contains auxillary functions. *)
module Helpers = struct
  let timestamp_to_utc_str timestamp =
    let open Unix in
    let tm = gmtime timestamp in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.tm_year)
      (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let ( >> ) f g x = g (f x)
  let log_info s = Logs_lwt.info (fun m -> m s)
end

(** Communication structure(s) and related. *)
module Comm = struct
  open Helpers

  type t =
    | Msg of { sent_at : float; payload : string }
    | Ack of { msg_sent_at : float }

  let comm_to_json : t -> Yojson.Basic.t = function
    | Ack { msg_sent_at } ->
        `Assoc [ ("type", `String "Ack"); ("msg_sent_at", `Float msg_sent_at) ]
    | Msg { sent_at; payload } ->
        `Assoc
          [
            ("type", `String "Msg");
            ("sent_at", `Float sent_at);
            ("payload", `String payload);
          ]

  let json_to_comm : Yojson.Basic.t -> t option = function
    | `Assoc [ ("type", `String "Ack"); ("msg_sent_at", `Float msg_sent_at) ] ->
        Some (Ack { msg_sent_at })
    | `Assoc
        [
          ("type", `String "Msg");
          ("sent_at", `Float sent_at);
          ("payload", `String payload);
        ] ->
        Some (Msg { sent_at; payload })
    | _ -> None

  let comm_to_str = comm_to_json >> Yojson.Basic.to_string
  let write_comm oc = comm_to_str >> Lwt_io.write_line oc
  let str_to_comm = Yojson.Basic.from_string >> json_to_comm
end

(** Patterns shared between the client and server *)
module Shared = struct
  open Helpers
  (* NOTE: sharing this much code is only possible because the
     requirements specify a client and server with mostly
     symmetrical functionality. Client and Server code should be
     separated if requirements change significantly. *)

  let rec _write_loop (oc : Lwt_io.output_channel) (stop : unit Lwt.t) :
      unit Lwt.t =
    Lwt.pick
      [
        (Lwt_io.read_line_opt Lwt_io.stdin
        >>= (function
              | Some line ->
                  Comm.write_comm oc
                  @@ Msg { payload = line; sent_at = Unix.gettimeofday () }
              | None -> log_info "Caught EOF (^D)")
        >>= fun () -> _write_loop oc stop);
        stop;
      ]

  let rec _read_loop (ic : Lwt_io.input_channel) (oc : Lwt_io.output_channel) =
    Lwt_io.read_line_opt ic
    >>= function
    | Some json_str ->
        (match Comm.str_to_comm json_str with
        | Some (Msg { sent_at; payload }) ->
            Lwt_io.printf "%s>>> %s\n" (timestamp_to_utc_str sent_at) payload
            >>= fun () -> Comm.write_comm oc @@ Ack { msg_sent_at = sent_at }
        | Some (Ack { msg_sent_at }) ->
            let roundtrip_ms = (Unix.gettimeofday () -. msg_sent_at) *. 1000. in
            Lwt_io.printf "Received & acknowledged in %sms\n"
              (string_of_float roundtrip_ms)
        | None -> log_info "Received None")
        >>= fun () -> _read_loop ic oc
    | None -> log_info "Connection closed by another participant" >>= return
end

module Client = struct
  let start_client : string -> int -> unit Lwt.t =
   fun ip_addr port ->
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
    let stop, stop_wakener = Lwt.task () in
    let server_address =
      Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, port)
    in
    Lwt_unix.connect sock server_address
    >>= fun () ->
    Lwt.async (fun () -> Shared._write_loop oc stop);
    Shared._read_loop ic oc
    >>= fun () ->
    Lwt.wakeup_later stop_wakener ();
    return ()
end

module Server = struct
  let _accept_connection conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    let handle_connection ic oc =
      let stop, stop_wakener = Lwt.task () in
      Lwt.async (fun () -> Shared._write_loop oc stop);
      Shared._read_loop ic oc
      >>= fun () ->
      Lwt.wakeup_later stop_wakener ();
      return ()
    in
    Lwt.on_failure (handle_connection ic oc) (fun e ->
        Logs.err (fun m -> m "%s" (Printexc.to_string e)));
    Helpers.log_info "New connection" >>= return

  let rec serve (sock : Lwt_unix.file_descr) : unit Lwt.t =
    Lwt_unix.accept sock >>= _accept_connection >>= fun () -> serve sock
end

let listen_address = Unix.inet_addr_any
let backlog = 10
let port = 12345

let create_socket =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  ignore @@ bind sock @@ ADDR_INET (listen_address, port);
  listen sock backlog;
  sock

let options =
  [
    ( "--server",
      Arg.Unit (fun () -> Lwt_main.run @@ Server.serve create_socket),
      "Start the server" );
    ( "--client",
      Arg.String
        (fun ip_addr -> Lwt_main.run @@ Client.start_client ip_addr port),
      "Start the client at the specified host IP address" );
  ]

let usage_msg = "Usage: ocamlchat (--server | --client [host])"

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  if Array.length Sys.argv <= 1 then Arg.usage options usage_msg
  else Arg.parse options (fun _ -> ()) usage_msg


