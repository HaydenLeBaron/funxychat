# README

```ocaml
let sGet = fun x y -> "CHAT"
(*
                  ,,___
        ..  ..   / ⌐■-■\  ファンキータイム! .---.
       /--'/--\  \-' W|       .----.      .'     '.
      /        \_/ / |      .'      '... '         '-.
    .'\  \__\  __.'.'     .'          ?-._
      )\ |  )\ |      _.'
     // \\ // \\
    ||_  \\|_  \\_
    '--' '--'' '--'
*)
```

## Problem Specification

Simple one on one chat.

Application should start in two modes:

- as a server, waiting for one client to connect or;
- as a client, taking an IP address (or hostname) of server to connect to.

After connection is established, user on either side (server and client) can send messages to the other side. After connection is terminated by the client, server continues waiting for another client. The receiving side should acknowledge every incoming message (automatically send back a "message received" indication), sending side should show the roundtrip time for acknowledgment. Wire protocol shouldn't make any assumptions on the message contents (e.g. allowed byte values, character encoding, etc).

## Setup Instructions

The following instructions assume you want to run the service on Amazon Linux 2 (I used an AWS Lightsail instance). Adapt as necessary.

**System requirements:**
- 2GB RAM (1GB not enough for compilation of some libraries)

**Setup env and dependencies:**

```
# install git
sudo yum install git

# install opam
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

sudo yum install patch.x86_64

# git clone funxychat, then cd to repo

opam init # select (y)

# You may have to install a c compiler if you don't have one
sudo yum install gcc

opam switch create 5.1.0

# Install dev stuff
opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release user-setup

# add to ~/.bashrc or equivalent
export PATH="~/.opam/5.1.0/bin/:$PATH"

# load bashrc
source ~/.bashrc

# Install libraries for funxychat
opam install yojson lwt

# On the machine running the server (AWS console if using lightsail), 
## open the firewall to the desired ports. I opened 12345-12350 to TCP and UDP connections.

```

## Usage

```bash
# start server
$ dune exec -- funxychat server [port-number]
## ex: $ dune exec -- funxychat server 12345

# start client
$ dune exec -- funxychat client [host-name/ip] [port-number]
## ex: $ dune exec -- funxychat client 44.228.147.265 12345
##     $ dune exec -- funxychat client localhost 12346
```

## Some Recommended Test cases

- Try starting the client without a server running. 
- While the server is running, try quitting the client forcibly and rejoining repeatedly.
- After a client has quit, try sending messages from the server. Upon another client connecting, these messages should be received by that client.
- Try sending various unicode characters.
- Try editing the encoding and decoding code on the client side and make sure a malicious client implementation can't cause the server to crash.
- When connected to a remote host, try disconnecting the client from wifi, send a message, then in ~10 seconds reconnect to wifi. The total roundtrip time of ~10s should be correctly displayed.

## Limitations

- Known issue: in some cases the server program won't restart immediately after a SIGTERM (^C)
because there may be an existing process running on this port that may take up to 30s to shut down. 
In this case, restart the server on a different port that is allowed by your server machine's 
firewall rules (or try again in a few seconds).
- The networking code is only intended to support a handshake and conversation between one client and one server at a time (though the client can quit and another or same client can quit and rejoin freely). The server will not prevent multiple clients from connecting, but in this case both clients will be able to message the server and the server's messages will alternate sending between the two clients.

