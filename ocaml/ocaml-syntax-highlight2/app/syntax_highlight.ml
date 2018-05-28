open! Core
open! Soup.Infix
open! Patdiff_lib

let process (src : string) : unit =

  let infile = Filename.realpath src in
  let outfile = infile ^ ".highlighted" in

  printf "+++ Reading %S\n" infile;
  printf "+++ Writing %S\n" outfile;

  Core.In_channel.with_file infile
    ~f:(fun c ->

        (* Parse the source document. *)
        let s = Core.In_channel.input_all c in
        let soup = Soup.parse s in

        (* Select all <code> elements that have empty class lists and
           replace them with <code class="code"> elements. *)
        soup $$ "code" |>
        Soup.filter (fun n -> List.is_empty (Soup.classes n)) |>
        Soup.iter (fun n ->
            Soup.replace n (
              Soup.create_element "code"
                ~class_:"code"
                ~inner_text:(Soup.require (Soup.leaf_text n))
            )
          )
        ;

        (* Select all <pre> elements, filter out those that have a
           <code> children and replace them with syntax highlighted
           ones. *)
        soup $$ "pre" |>
        Soup.filter (fun n -> Soup.count (n $$ "code") = 0) |>
        Soup.iter (fun n ->
            let buf = Buffer.create 1024 in
            let code = Soup.require (Soup.leaf_text n) in
            (Odoc_ocamlhtml.html_of_code buf ~with_pre:true code;
             let node = Soup.parse (Buffer.contents buf) in
             Soup.clear n;
             Soup.append_child n node)
          )
        ;

        (* Extract the html text of the new document and write it to
           disk. *)
        let s' = Soup.to_string soup in
        Core.Out_channel.with_file outfile
          ~f:(fun w -> Core.Out_channel.output_string w s')
        ;

        (* Print a patience diff of the two documents to the
           console. *)
        let from_ = {Patdiff_core.name="before"; text=s } in
        let to_   = {Patdiff_core.name=" after"; text=s'} in
        printf "%s\n" (Patdiff_core.patdiff ~keep_ws:true ~from_ ~to_ ())

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
