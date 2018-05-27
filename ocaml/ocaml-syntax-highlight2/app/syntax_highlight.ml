open! Core
open! Soup.Infix

let process (src : string) : unit =
  Core.In_channel.with_file src
    ~f:(fun c ->
        let s = Core.In_channel.input_all c in
        let soup = Soup.parse s in
        soup $$ "pre" |>
        Soup.iter (fun n ->
            let buf = Buffer.create 1024 in
            let code = Soup.require (Soup.leaf_text n) in
            (Odoc_ocamlhtml.html_of_code buf ~with_pre:true code;
             let node = Soup.parse (Buffer.contents buf) in
             Soup.clear n;
             Soup.append_child n node)
          )
      ;   printf "%s" (Soup.to_string soup)
      )

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"syntax highlight"
    [%map_open
      let src = anon ("SRC" %: string) in
      fun () ->
        match Sys.file_exists src with
        | `Yes -> process src
        | `No | `Unknown ->
          failwith (
            Printf.sprintf "Bad argument : '%s' does not exist" src
          )
    ]

let () =
  try Command.run ~version:"0.1" command
  with e -> eprintf "%s\n" (Exn.to_string e)
