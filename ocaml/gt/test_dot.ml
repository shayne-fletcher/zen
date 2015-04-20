module Dotfile = struct
  let open_write (name : string) (buf : string) : unit=
    let ch = open_out name in
    output ch buf 0 (String.length buf) ; close_out ch

  let image_of_dotfile (dotfile : string) =
    let pid = Unix.create_process "dot.exe" [|"dot.exe"; "-Tpng"; "-O"; dotfile|] Unix.stdin Unix.stdout Unix.stderr in
    let _ = Unix.waitpid [] pid in 
    ()

  let render_dotfile (dotfile : string) : unit =
    let _ = Unix.create_process "dotty.exe" [|"dotty.exe"; dotfile|] Unix.stdin Unix.stdout Unix.stderr in
    ()
end

open Gt
open Dotfile

module G : Directed_graph.S with type node = Char.t = Directed_graph.Make (Char)

let g : G.t =
  G.of_adjacency
  [
    'a', ['b']           ;
    'b', ['e'; 'f'; 'c'] ;
    'c', ['d'; 'g']      ;
    'd', ['c'; 'h']      ;
    'e', ['a'; 'f']      ;
    'f', ['g']           ;
    'g', ['f'; 'h']      ;
    'h', ['h']           ;
  ]

let s = Filename.temp_file "" ".dot"
let () = 
  open_write s 
   (G.to_dot_digraph (fun (c : char) -> Bytes.to_string (Bytes.make 1 c)) g); 
  render_dotfile s
