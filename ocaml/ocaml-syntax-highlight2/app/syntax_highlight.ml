open! Core
open! Patdiff_lib
open! Soup.Infix

let process (src : string) : unit =
  let infile = Filename.realpath src in
  let outfile = infile ^ ".highlighted" in
  printf "+++ Reading %S\n" infile;
  printf "+++ Writing %S\n" outfile;
  Core.In_channel.with_file infile
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
          );
        let s' = Soup.to_string soup in
        let from_ = {Patdiff_core.name="before"; text=s } in
        let to_   = {Patdiff_core.name=" after"; text=s'} in
        Core.Out_channel.with_file outfile
          ~f:(fun w -> Core.Out_channel.output_string w s')
        ; printf "%s\n" (Patdiff_core.patdiff ~keep_ws:true ~from_ ~to_ ())
      )

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"ocaml syntax highlight preformatted blocks in html file"
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
