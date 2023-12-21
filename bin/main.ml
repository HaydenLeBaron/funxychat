 open Lwt

 let messages: string list ref = ref []
 
 let listen_address = Unix.inet_addr_any
 let port = 12345
 let backlog = 10
 
 
 let handle_message msg =
     match msg with
     | _  -> messages := string_of_float (Unix.gettimeofday ()) :: !messages ; Unix.time()

let handle_connection ic oc () =
    let stop, stop_wakener = Lwt.task () in
    let rec write_loop () =
        Lwt.pick [ (if Lwt_io.is_closed oc then
                        Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
                    else
                    Lwt_io.read_line_opt Lwt_io.stdin >>= function
                    | Some msg ->
                        if Lwt_io.is_closed oc then
                            Logs_lwt.info (fun m -> m "Connection closed 1") >>= return
                        else
                            Lwt_io.write_line oc msg >>= fun () -> write_loop ()
                    | None -> Logs_lwt.info (fun m -> m "Connection closed 2") >>= return);
                    stop]
    in
    let rec read_loop () =
        if Lwt_io.is_closed oc then
            Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
        else
            Lwt_io.read_line_opt ic >>= function
            | Some msg -> 
                let incoming_msg_sent_at = handle_message msg in
                Lwt_io.printf "Message received: %s\n" msg >>= fun () ->
                Lwt_io.write_line oc (string_of_float ((Unix.gettimeofday() -. incoming_msg_sent_at) *. 100.)) >>= read_loop
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
                    | Some msg ->
                        if Lwt_io.is_closed oc then
                            Logs_lwt.info (fun m -> m "Connection closed 5") >>= return
                        else
                            Lwt_io.write_line oc msg >>= fun () -> write_loop ()
                    | None -> Logs_lwt.info (fun m -> m "Connection closed 6 ^D (EOF)") >>= return);
                    stop]
    in
    let rec read_loop () =
        if Lwt_io.is_closed oc then
            Logs_lwt.info (fun m -> m "Connection closed 0") >>= return
        else
            Lwt_io.read_line_opt ic >>= function
            | Some msg -> 
                Lwt_io.printf "Message received: %s\n" msg >>= read_loop
            | None -> Logs_lwt.info (fun m -> m "Connection closed 3") >>= return
    in
    (* Lwt.async write_loop; *)
    (* read_loop () >>= fun () -> Lwt.wakeup_later stop_wakener (); return () *)
    Lwt.join [write_loop (); read_loop ()] >>= fun () -> Lwt.wakeup_later stop_wakener (); return ()

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

