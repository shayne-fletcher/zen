(* Uppercase server from
https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora187.html#toc280
organized in the "programmation modulaire generique" style

Compile with `ocamlc -thread -o serv_up.exe unix.cma threads.cma serv_up.ml`.
*)

module type PORT = sig
  val port_number : int
end

module type ADDRESS = sig
  type t
  val get : unit -> t
end

module type SOCKET_ADDRESS_F =
  functor (P : PORT) -> ADDRESS with type t = Unix.sockaddr

module type SOCKET = sig
  val get : unit -> Unix.file_descr
  val close : unit -> unit
end

module type SOCKET_F =
  functor (A : ADDRESS with type t = Unix.sockaddr) -> SOCKET

module type SERVER = sig
  val run : (in_channel -> out_channel -> 'a) -> unit
end

module type SERVER_F = functor (S : SOCKET) -> SERVER

module Internet_address : SOCKET_ADDRESS_F =
  functor (P : PORT) -> struct
    type t = Unix.sockaddr
    let get () =
      Unix.ADDR_INET
        ((Unix.gethostbyname(
              Unix.gethostname())
         ).Unix.h_addr_list.(0), P.port_number)
  end

module TCP_socket : SOCKET_F =
  functor (A : ADDRESS with type t = Unix.sockaddr) -> struct

    let fd =
      let init_fd () =
        let sa = A.get () in
        let domain = Unix.domain_of_sockaddr sa in
        let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
        Unix.bind sock sa;
        Unix.listen sock 3;
        sock in
      init_fd ()

    let get () = fd
    let close () = Unix.close fd
  end

module Server : SERVER_F =
  functor (S : SOCKET) -> struct
    let thread_func f s =
      let in_ch = Unix.in_channel_of_descr s
      and out_ch = Unix.out_channel_of_descr s in
      f in_ch out_ch;
      (* close_in in_ch; *)
      close_out out_ch

    let run f =
      let s = S.get () in
      while true do
        let (s, caller) = Unix.accept s in
        ignore (Thread.create (fun () -> thread_func f s) ())
      done;
      S.close ()
  end

let uppercase_service (ic : in_channel) (oc : out_channel) : unit =
  try while true do
      let s = input_line ic in
      let r = String.uppercase_ascii s
      in output_string oc (r ^ "\n"); flush oc
    done
  with
  | _ ->Printf.printf "End of text\n"; flush stdout

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
