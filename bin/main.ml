open Lwt

(*BKMRK/INPROG: refactor code *)
(*BKMRK/TODO: make multiclient by being able to vary the port (I think) *)
(*BKMRK/TODO: test badly implemented client sending Json with the wrong field names and/or types. I think it will take the server down.*)
(*BKMRK/TODO: get client to pass in name to put in prompt for eventual multi-client support *)
(*BKMRK/TODO: implement hostname resolution *)
(*BKMRK/FIXME: problem where sometimes the old server is running on the old port (could fix with port variation) *)

let port = 12345

(** Contains auxillary functions. *)
module Helpers = struct
  let timestamp_to_utc_str timestamp =
    let open Unix in
    let tm = gmtime timestamp in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.tm_year)
      (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let ( >> ) f g x = g (f x)
end

(** Contains communication structure(s) and related. *)
module Comm = struct
  open Helpers

  type t =
    | Msg of { sent_at : float; payload : string }
    | Ack of { msg_sent_at : float }

  let comm_to_json : t -> Yojson.Basic.t = function
    | Ack { msg_sent_at } ->
        `Assoc [ ("type", `String "Ack"); ("msg_sent_at", `Float msg_sent_at) ]
    | Msg { sent_at ; payload  } ->
        `Assoc
          [
            ("type", `String "Msg");
            ("sent_at", `Float sent_at);
            ("payload", `String payload);
          ]

  let json_to_comm : string -> t option =
    Yojson.Basic.from_string >> function
    | `Assoc [ ("type", `String "Ack"); ("msg_sent_at", `Float msg_sent_at) ] ->
        Some (Ack { msg_sent_at })
    | `Assoc
        [
          ("type", `String "Msg");
          ("sent_at", `Float sent_at);
          ("payload", `String payload);
        ] ->
        Some (Msg { sent_at ; payload })
    | _ -> None
end

(*BKMRK/NOTE: Refactored to HERE ---- *)

module Client = struct
  open Helpers
  open Comm

  let create_client ip_addr port =
    let server_address =
      Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, port)
    in
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect sock server_address >>= fun () ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
    let stop, stop_wakener = Lwt.task () in
    let rec write_loop () =
      Lwt.pick
        [
          (if Lwt_io.is_closed oc then
             Logs_lwt.info (fun m -> m "Connection closed 4") >>= return
           else
             Lwt_io.read_line_opt Lwt_io.stdin >>= function
             | Some line ->
                 Msg { payload = line; sent_at = Unix.gettimeofday () }
                 |> comm_to_json |> Yojson.Basic.to_string
                 |> fun json_str ->
                 if Lwt_io.is_closed oc then
                   Logs_lwt.info (fun m -> m "Connection closed 5") >>= return
                 else
                   json_str |> Lwt_io.write_line oc >>= fun () -> write_loop ()
             | None ->
                 Logs_lwt.info (fun m -> m "Connection closed 6 ^D (EOF)")
                 >>= return);
          stop;
        ]
    in
    let rec read_loop () =
      if Lwt_io.is_closed oc then
        Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
      else
        Lwt_io.read_line_opt ic >>= function
        | Some json_str -> (
            let comm = json_to_comm json_str in
            comm |> fun comm ->
            match comm with
            | Some (Msg { payload; sent_at }) ->
                Lwt_io.printf "%s>>> %s\n"
                  (timestamp_to_utc_str sent_at)
                  payload
                >>= fun () ->
                Ack { msg_sent_at = sent_at }
                |> comm_to_json |> Yojson.Basic.to_string
                |> Lwt_io.write_line oc >>= read_loop
            | Some (Ack { msg_sent_at }) ->
                let roundtrip_ms =
                  (Unix.gettimeofday () -. msg_sent_at) *. 1000.
                in
                Lwt_io.printf "Recieved + acknowledged in %sms\n"
                  (string_of_float roundtrip_ms)
                >>= read_loop
            | None ->
                Logs_lwt.info (fun m -> m "BKMRK/888 Recieved None")
                >>= read_loop)
        | None ->
            Logs_lwt.info (fun m -> m "The server closed the connection")
            >>= return
    in
    Lwt.async write_loop;
    read_loop () >>= fun () ->
    Lwt.wakeup_later stop_wakener ();
    return ()
end

module Server = struct
  open Comm

  let listen_address = Unix.inet_addr_any
  let backlog = 10

  let handle_connection ic oc () =
    let stop, stop_wakener = Lwt.task () in
    let rec write_loop () =
      Lwt.pick
        [
          (if Lwt_io.is_closed oc then
             Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
           else
             Lwt_io.read_line_opt Lwt_io.stdin >>= function
             | Some line ->
                 if Lwt_io.is_closed oc then
                   Logs_lwt.info (fun m -> m "Connection closed 1") >>= return
                 else
                   Comm.Msg { payload = line; sent_at = Unix.gettimeofday () }
                   |> Comm.comm_to_json |> Yojson.Basic.to_string
                   |> Lwt_io.write_line oc >>= write_loop
             | None ->
                 Logs_lwt.info (fun m -> m "Connection closed 2") >>= return);
          stop;
        ]
    in
    let rec read_loop () =
      if Lwt_io.is_closed oc then
        Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
      else
        Lwt_io.read_line_opt ic >>= function
        | Some json_str -> (
            let comm = json_to_comm json_str in
            comm |> fun comm ->
            match comm with
            | Some (Msg { payload; sent_at }) ->
                Lwt_io.printf "%s>>> %s\n"
                  (Helpers.timestamp_to_utc_str sent_at)
                  payload
                >>= fun () ->
                Ack { msg_sent_at = sent_at }
                |> comm_to_json |> Yojson.Basic.to_string
                |> Lwt_io.write_line oc >>= read_loop
            | Some (Ack { msg_sent_at }) ->
                let roundtrip_ms =
                  (Unix.gettimeofday () -. msg_sent_at) *. 1000.
                in
                Lwt_io.printf "Recieved + acknowledged in %sms\n"
                  (string_of_float roundtrip_ms)
                >>= read_loop
            | None ->
                Logs_lwt.info (fun m -> m "BKMRK/777 Recieved None")
                >>= read_loop)
        | None ->
            Logs_lwt.info (fun m -> m "Client closed the connection") >>= return
    in
    Lwt.async write_loop;
    read_loop () >>= fun () ->
    Lwt.wakeup_later stop_wakener ();
    return ()

  let accept_connection conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    Lwt.on_failure (handle_connection ic oc ()) (fun e ->
        Logs.err (fun m -> m "%s" (Printexc.to_string e)));
    Logs_lwt.info (fun m -> m "New connection") >>= return

  let create_socket () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    ignore @@ bind sock @@ ADDR_INET (listen_address, port);
    listen sock backlog;
    sock

  let create_server sock =
    let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
    serve
end

let sock = Server.create_socket ()

let options =
  [
    ( "--server",
      Arg.Unit (fun () -> Lwt_main.run @@ Server.create_server sock ()),
      "Start the server" );
    ( "--client",
      Arg.String
        (fun ip_addr -> Lwt_main.run @@ Client.create_client ip_addr port),
      "Start the client at the specified host IP address" );
  ]

let usage_msg = "Usage: ocamlchat (--server | --client [host])"

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  if Array.length Sys.argv <= 1 then Arg.usage options usage_msg
  else Arg.parse options (fun _ -> ()) usage_msg


