 open Lwt

 (* let messages: string list ref = ref [] *)

 type comm =
  | Msg of { payload : string; sent_at : float }
  | Ack of { msg_sent_at : float }
 
 let listen_address = Unix.inet_addr_any
 (*BKMRK/TODO: make multiclient by being able to vary the port (I think) *)
 let port = 12345
 let backlog = 10

 (*BKMRK/TODO: test badly implemented client sending Json with the wrong field names and/or types. I think it will take the server down.*)
 
 
 (*let handle_comm comm =
     match msg with
     | Msg -> messages := string_of_float (Unix.gettimeofday ()) :: !messages ; Unix.time()
     | Ack  -> *)
let comm_to_json = function
    | Ack { msg_sent_at } ->
        `Assoc [ ("type", `String "Ack"); ("msg_sent_at", `Float msg_sent_at) ]
    | Msg { payload; sent_at } ->
        `Assoc
        [
            ("type", `String "Msg");
            ("payload", `String payload);
            ("sent_at", `Float sent_at);
        ]

(*let comm_to_json = function
| Ack { msg_sent_at } ->
    let msg_sent_at = match msg_sent_at with
        | None -> 0.0  (* Replace null with a default value *)
        | Some v -> v
    in
    `Assoc [ ("type", `String "Ack"); ("msg_sent_at", `Float msg_sent_at) ]
| Msg { payload; sent_at } ->
    let sent_at = match sent_at with
        | None -> 0.0  (* Replace null with a default value *)
        | Some v -> v
    in
    `Assoc
    [
        ("type", `String "Msg");
        ("payload", `String payload);
        ("sent_at", `Float sent_at);
    ]*)

(*let json_to_comm json =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string json in
    match json |> member "type" |> to_string with
    | "Ack" ->
        Some (Ack { msg_sent_at = json |> member "sent_at" |> to_float })
    | "Msg" ->
        Some (Msg
            {
            payload = json |> member "payload" |> to_string;
            sent_at = json |> member "sent_at" |> to_float;
            })
    | _ -> None*)

let json_to_comm json =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string json in
    match json |> member "type" |> to_string with
    | "Ack" ->
        let msg_sent_at = json |> member "msg_sent_at" in
        (match msg_sent_at with
        | `Null -> None
        | `Float f -> Some (Ack { msg_sent_at = f })
        | _ -> None)
    | "Msg" ->
        let payload = json |> member "payload" |> to_string in
        let sent_at = json |> member "sent_at" in
        (match sent_at with
        | `Null -> None
        | `Float f -> Some (Msg { payload = payload; sent_at = f })
        | _ -> None)
    | _ -> None

let handle_connection ic oc () =
    let stop, stop_wakener = Lwt.task () in
    let rec write_loop () =
        Lwt.pick [ (if Lwt_io.is_closed oc then
                        Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
                    else
                    Lwt_io.read_line_opt Lwt_io.stdin >>= function
                    | Some line ->
                        if Lwt_io.is_closed oc then
                            Logs_lwt.info (fun m -> m "Connection closed 1") >>= return
                        else
                            Msg { payload=line; sent_at=Unix.gettimeofday () }
                            |> comm_to_json |> Yojson.Basic.to_string
                            |> Lwt_io.write_line oc >>= write_loop
                    | None -> Logs_lwt.info (fun m -> m "Connection closed 2") >>= return);
                    stop]
    in
    let rec read_loop () =
        if Lwt_io.is_closed oc then
            Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
        else
            Lwt_io.read_line_opt ic >>= function
            | Some json_str -> 
                (let comm = json_to_comm json_str in
                comm |> fun comm ->
                    match comm with
                    | Some(Msg { payload; sent_at }) -> 
                            Lwt_io.printf "Message received: %s, sent_at %s\n" payload (string_of_float sent_at) >>=
                            fun () -> (Ack { msg_sent_at=sent_at }
                            |> comm_to_json |> Yojson.Basic.to_string
                            |> Lwt_io.write_line oc)
                        >>= read_loop
                    | Some(Ack { msg_sent_at }) ->
                        Lwt_io.printf "Ack received: %s\n" (string_of_float msg_sent_at) >>= read_loop
                    | None -> Logs_lwt.info (fun m -> m "BKMRK/777 Recieved None") >>= read_loop)
            | None -> Logs_lwt.info (fun m -> m "Connection closed 3") >>= return
    in
     Lwt.async write_loop; 
     read_loop () >>= fun () -> Lwt.wakeup_later stop_wakener (); return ()


 let accept_connection conn =
     let fd, _ = conn in
     let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
     let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
     Lwt.on_failure (handle_connection ic oc ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
     Logs_lwt.info (fun m -> m "New connection") >>= return
  
 let create_socket () =
     let open Lwt_unix in
     let sock = socket PF_INET SOCK_STREAM 0 in
     ignore @@ bind sock @@ ADDR_INET(listen_address, port);
     listen sock backlog;
     sock
 
 let create_server sock =
     let rec serve () =
         Lwt_unix.accept sock >>= accept_connection >>= serve
     in serve


let create_client ip_addr port =
    let server_address = Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, port) in
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect sock server_address >>= fun () ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
    let stop, stop_wakener = Lwt.task () in
    let rec write_loop () =
        Lwt.pick [ (if Lwt_io.is_closed oc then
                        Logs_lwt.info (fun m -> m "Connection closed 4") >>= return
                    else
                    Lwt_io.read_line_opt Lwt_io.stdin >>= function
                    | Some line -> Msg { payload=line; sent_at=Unix.gettimeofday () } 
                    |> comm_to_json |> Yojson.Basic.to_string
                    (* |> Lwt_io.write_line oc >>= fun () -> write_loop () *)
                        |> fun json_str -> 
                        if Lwt_io.is_closed oc then
                            Logs_lwt.info (fun m -> m "Connection closed 5") >>= return
                        else
                            json_str |> Lwt_io.write_line oc >>= fun () -> write_loop ()
                    | None -> Logs_lwt.info (fun m -> m "Connection closed 6 ^D (EOF)") >>= return);
                    stop]
    in
    let rec read_loop () =
        if Lwt_io.is_closed oc then
            Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
        else
            Lwt_io.read_line_opt ic >>= function
            | Some json_str -> 
                (let comm = json_to_comm json_str in
                comm |> fun comm ->
                match comm with
                | Some(Msg { payload; sent_at }) -> 
                            Lwt_io.printf "Message received: %s, sent_at %s\n" payload (string_of_float sent_at) >>=
                            fun () -> (Ack { msg_sent_at=sent_at }
                            |> comm_to_json |> Yojson.Basic.to_string
                            |> Lwt_io.write_line oc) >>= read_loop
                | Some( Ack { msg_sent_at } ) -> 
                    Lwt_io.printf "Ack received: %s\n" (string_of_float msg_sent_at) >>= read_loop
                | None -> Logs_lwt.info (fun m -> m "BKMRK/888 Recieved None") >>= read_loop
                )
                (* Lwt_io.printf "Message received: %s\n" msg >>= read_loop *)
            | None -> Logs_lwt.info (fun m -> m "Connection closed 999") >>= return
                in
    Lwt.async write_loop;
    read_loop () >>= fun () -> Lwt.wakeup_later stop_wakener (); return ()
    (* Lwt.join [write_loop (); read_loop ()] >>= fun () -> Lwt.wakeup_later stop_wakener (); return () *)

let sock = create_socket ()

let options =
[
    ( "--server",
    Arg.Unit (fun () -> (Lwt_main.run @@ create_server sock ())),
    "Start the server" );
    ( "--client",
    Arg.String (fun ip_addr -> Lwt_main.run @@ (create_client ip_addr port)),
    "Start the client at the specified host IP address" );
]

      let usage_msg = "Usage: ocamlchat (--server | --client [host])"
 
 let () =
     let () = Logs.set_reporter (Logs.format_reporter ()) in
     let () = Logs.set_level (Some Logs.Info) in
     if Array.length Sys.argv <= 1 then Arg.usage options usage_msg
     else Arg.parse options (fun _ -> ()) usage_msg


