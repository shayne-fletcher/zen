open! Core
open! Soup.Infix
open! Patdiff_lib
open! Syntax_highlight

let process (src : string) : unit =
  let infile = Filename.realpath src in
  let outfile = infile ^ ".highlighted" in
  printf "[Info] Reading %S\n" infile;
  printf "[Info] Writing %S\n" outfile;
  Core.In_channel.with_file infile
    ~f:(fun c ->
        let s = Core.In_channel.input_all c in
        let s' = Syntax_highlight_core.highlight s in
        let from_ = { Patdiff_core.name="before"; text=s } in
        let to_   = { Patdiff_core.name=" after"; text=s'} in
        printf "%s\n" (Patdiff_core.patdiff ~keep_ws:true ~from_ ~to_ ());
        Core.Out_channel.with_file outfile
          ~f:(fun w -> Core.Out_channel.output_string w s')
      )

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"syntax highlight ocaml code in html"
    [%map_open
      let src = anon ("FILE" %: string) in
      fun () ->
        match Sys.file_exists src with
        | `Yes -> process src
        | `No | `Unknown ->
          failwith (
            Printf.sprintf "The source file '%s' does not exist" src
          )
    ]

let () =
  try Command.run ~version:"0.1" command
  with e -> eprintf "%s\n" (Exn.to_string e)
