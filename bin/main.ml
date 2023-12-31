open Lwt
open Lwt.Infix

(** Contains auxillary functions. *)
module Helpers = struct
  let timestamp_to_utc_str timestamp =
    let open Unix in
    let tm = gmtime timestamp in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.tm_year)
      (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let ( >> ) f g x = g (f x)
  let log_info s = Logs_lwt.info (fun m -> m s)

  let print_splash_screen () =
    Lwt_io.print
      {|
              ,,___
    ..  ..   / ⌐■-■\  ファンキータイム!    .---.
   /--'/--\  \-' W|       .----.      .'     '.
  /        \_/ / |      .'      '... '         '-.
.'\  \__\  __.'.'     .'          ?-._
  )\ |  )\ |      _.'
 // \\ // \\
||_  \\|_  \\_
'--' '--'' '--'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let sGet = fun (x y) -> ....
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
|}
end

(** Communication structure(s) and their (en/de)coding. *)
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
              | None -> log_info "[Caught EOF (^D)]")
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
            Lwt_io.printf "[Received & acknowledged in %sms]\n"
              (string_of_float roundtrip_ms)
        | None -> log_info "[Received None]")
        >>= fun () -> _read_loop ic oc
    | None -> log_info "[Connection closed by other participant]" >>= return
end

module Client = struct
  let start_client : string -> int -> unit Lwt.t =
   fun host port ->
    let server_addr =
      try (Unix.gethostbyname host).Unix.h_addr_list.(0)
      with Not_found -> failwith ("Unknown host: " ^ host)
    in
    let sock = Unix.ADDR_INET (server_addr, port) in
    Lwt_io.open_connection sock
    >>= fun (ic, oc) ->
    let stop, stop_wakener = Lwt.task () in
    Lwt.async (fun () -> Shared._write_loop oc stop);
    Shared._read_loop ic oc
    >>= fun () ->
    Lwt.wakeup_later stop_wakener ();
    return ()
end

module Server = struct
  let create_socket port =
    let open Lwt_unix in
    let listen_address = Unix.inet_addr_any in
    let backlog = 10 in
    let sock = socket PF_INET SOCK_STREAM 0 in
    ignore @@ bind sock @@ ADDR_INET (listen_address, port);
    listen sock backlog;
    sock

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

  let rec serve (sock : Lwt_unix.file_descr) =
    Lwt_unix.accept sock >>= _accept_connection >>= fun () -> serve sock
end

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  match Sys.argv with
  | [| _; "server"; port |] ->
      let port = int_of_string port in
      Lwt_main.run
        (Helpers.print_splash_screen ()
        >>= fun () -> Server.serve (Server.create_socket port))
  | [| _; "client"; host; port |] ->
      let port = int_of_string port in
      Lwt_main.run
        (Helpers.print_splash_screen ()
        >>= fun () -> Client.start_client host port)
  | _ ->
      Printf.printf
        "Usage:\n\tfunxychat server <port>\n\tfunxychat client <host> <port>\n"

