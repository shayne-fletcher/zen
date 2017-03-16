(*Sen Han - uses 'fork'*)

module type PORT = sig
  val port_number : int
end

module type ADDRESS = sig
  type t
  val get : unit -> int
end

module type SOCKET_ADDRESS_F = functor (P : PORT) -> ADDRESS with type t = Unix.sockaddr

module type SOCKET = sig
  val get : unit -> Unix.file_descr
  val close : unit -> unit
end

module type SOCKET_F = functor (A : ADDRESS with type t = Unix.sockaddr) -> SOCKET

module type SERVER = sig
  val run : (in_channel -> out_channel -> 'a) -> unit
end

module type SERVER_F = functor (S : SOCKET) -> SERVER

module Internet_address : SOCKET_ADDRESS_F =
  functor (P : PORT) -> struct
    type t = Unix.sockaddr
    let get = fun () -> P.port_number
  end

module TCP_socket : SOCKET_F =
  functor (A : ADDRESS with type t = Unix.sockaddr) -> struct
    let get = fun () ->
      let sock_fdecr = (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0) in
      let sock_address = Unix.ADDR_INET( Unix.inet_addr_of_string "127.0.0.1", (A.get ()) )
      in
      Unix.bind sock_fdecr sock_address;
      sock_fdecr
    let close = fun () -> ()
  end

module Server : SERVER_F =
  functor (S : SOCKET) -> struct
    let run server_func =
      let sock = S.get () in
      Unix.listen sock 3;
      while true do
        print_string("lkajdfkl \n");
        let (s, caller) = Unix.accept sock
        in match Unix.fork() with
        | 0 ->
          if Unix.fork() <> 0 then exit 0;
          let inchan  = Unix.in_channel_of_descr s in
          let outchan  = Unix.out_channel_of_descr s
          in server_func inchan outchan;
          close_in inchan;
          close_out outchan;
        | id -> Unix.close s; ignore(Unix.waitpid [] id)
      done
  end

let uppercase_service (ic : in_channel) (oc : out_channel) : unit =
  try while true do
      let s = input_line ic in
      let r = String.uppercase s
      in output_string oc (r ^ "\n"); flush oc
    done
  with
  | _ -> Printf.printf "End of text\n"; flush stdout

let main f =
  if Array.length Sys.argv < 2 then Printf.eprintf "usage : serv_up port\n"
  else try
    let module S =
      Server (
        TCP_socket (
          Internet_address (
            struct
              let port_number = int_of_string Sys.argv.(1)
            end))) in
    S.run f
  with
  | Failure _ -> Printf.eprintf "serv_up : bad port number\n"

let _ = Unix.handle_unix_error main uppercase_service

(**)

(*Viet Li*)

module type PORT = sig
  val port_number : int
end

module type ADDRESS = sig
  type t
  val get : unit -> t
end

module type SOCKET_ADDRESS_F = functor (P : PORT) -> ADDRESS with type t = Unix.sockaddr

module type SOCKET = sig
  val get : unit -> Unix.file_descr
  val close : unit -> unit
end

module type SOCKET_F = functor (A : ADDRESS with type t = Unix.sockaddr) -> SOCKET

module type SERVER = sig
  val run : (in_channel -> out_channel -> 'a) -> unit
end

module type SERVER_F = functor (S : SOCKET) -> SERVER

(* ********************************************************** *)

module Internet_address : SOCKET_ADDRESS_F =
  functor (P : PORT) -> struct
    type t = Unix.sockaddr

    let m_sockaddr =
      Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", P.port_number)

    let get () = m_sockaddr
  end

module TCP_socket : SOCKET_F =
  functor (A : ADDRESS with type t = Unix.sockaddr) -> struct
      let m_fd = Unix.(socket PF_INET SOCK_STREAM 0)

      let get () =
        Unix.(setsockopt m_fd SO_REUSEADDR true);
        Unix.(setsockopt_optint m_fd SO_LINGER (Some 3));
        Unix.bind m_fd (A.get ());
        m_fd

      and close () = Unix.close m_fd
  end

module Server : SERVER_F =
  functor (S : SOCKET) -> struct
  let tfunc (f, client_sock) =
    Unix.(setsockopt_optint client_sock SO_LINGER (Some 3));
    f (Unix.in_channel_of_descr client_sock) (Unix.out_channel_of_descr client_sock);
    begin
      try
        Unix.(shutdown client_sock SHUTDOWN_ALL);
      with
      | _ -> ()
    end

    let rec process sock f =
      let client_sock, client_sockaddr = Unix.accept sock in
      ignore (Thread.create tfunc (f, client_sock));
      process sock f

    let run f =
      let sock = S.get () in
      Unix.listen sock 64;
      process sock f
  end

let uppercase_service (ic : in_channel) (oc : out_channel) : unit =
  try
    while true do
      let s = input_line ic in
      let r = String.uppercase_ascii s
      in output_string oc (r ^ "\n"); flush oc
    done
  with
  | End_of_file -> Printf.printf "End of text\n"; flush stdout
  | Sys_error s -> Printf.eprintf "Got Sys_error: %s\n" s; flush stderr
  | _ -> Printf.eprintf "Unknown error!\n"; flush stderr

(* ********************************************************** *)

let main f =
  if Array.length Sys.argv < 2 then Printf.eprintf "usage : serv_up port\n"
  else try
    let module S =
      Server (
        TCP_socket (
          Internet_address (
            struct
              let port_number = int_of_string Sys.argv.(1)
            end))) in
    S.run f
  with
  | Failure _ -> Printf.eprintf "serv_up : bad port number\n"

let _ =
  Unix.handle_unix_error main uppercase_service

(*Joel Bjornson*)

module type PORT = sig
  val port_number : int
end

module type ADDRESS = sig
  type t
  val get : unit -> t
end

module type SOCKET_ADDRESS_F = functor (P : PORT) -> ADDRESS with type t = Unix.sockaddr

module type SOCKET = sig
  val get : unit -> Unix.file_descr
  val close : unit -> unit
end

module type SOCKET_F = functor (A : ADDRESS with type t = Unix.sockaddr) -> SOCKET

module type SERVER = sig
  val run : (in_channel -> out_channel -> 'a) -> unit
end

module type SERVER_F = functor (S : SOCKET) -> SERVER

module Internet_address : SOCKET_ADDRESS_F = functor (P : PORT) -> struct

  type t = Unix.sockaddr

  let get () =
    let address = Unix.inet_addr_of_string "127.0.0.1" in
    Unix.ADDR_INET (address, P.port_number)
end

module TCP_socket : SOCKET_F = functor (A : ADDRESS with type t = Unix.sockaddr) -> struct

  let get () =
    let addrs = A.get () in
    let domain = Unix.domain_of_sockaddr addrs in
    let sock = Unix.socket domain SOCK_STREAM 0 in
    Unix.bind sock addrs;
    sock

  let close () = Unix.close @@ get ()

end

module Server : SERVER_F = functor (S : SOCKET) -> struct

  let run serve =
    let sock = S.get () in
    Unix.listen sock 3;
    while true do
      let (s, caller) = Unix.accept sock in
      let handle () =
        let inchan = Unix.in_channel_of_descr s in
        let outchan = Unix.out_channel_of_descr s in
        serve inchan outchan;
        close_in inchan;
        close_out outchan;
        Thread.exit ()
      in
      Thread.create handle ()
    done
end

let uppercase_service (ic : in_channel) (oc : out_channel) : unit =
  try
    while true do
      let s = input_line ic in
      let r = String.uppercase_ascii s in
      output_string oc (r ^ "\n");
      flush oc
    done
  with
  | e ->
    raise e
    Printf.printf "End of text\n";
    flush stdout

let main f =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "usage : serv_up port\n"
  else
    try
      let module IA =
        Internet_address (
          struct
            let port_number = int_of_string Sys.argv.(1)
          end
        )
      in
      let module TS = TCP_socket (IA) in
      let module S = Server (TS) in
      S.run f;
    with
    | Failure _ ->
      Printf.eprintf "serv_up : bad port number\n"

let _ = Unix.handle_unix_error main uppercase_service
