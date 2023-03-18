type args =
  | Args of {
      lib : string;
      srcdir : string;
      out : string;
      srcfiles : string list;
    }

let command : string ref = ref ""
let lib : string ref = ref ""
let out : string ref = ref ""
let srcdir : string ref = ref ""
let srcfiles : string list ref = ref []

let read_args () : args =
  let lib = !lib in
  let srcdir = Filename.concat !srcdir "" in
  let out = !out in
  let srcfiles =
    List.filter
      (fun f -> Sys.file_exists f && String.starts_with ~prefix:srcdir f)
      !srcfiles
  in
  Args { lib; srcdir; out; srcfiles }

let read_file (fn : string) : string =
  In_channel.with_open_bin fn (fun inp -> In_channel.input_all inp)

let write_file (fn : string) (cs : string) : unit =
  Out_channel.with_open_bin fn (fun outp -> Out_channel.output_string outp cs)

let copy_file (src : string) (dst : string) : unit =
  write_file dst (read_file src)

let rec mkdirs (path : string) : unit =
  if not (Sys.file_exists path) then (
    mkdirs (Filename.dirname path);
    Sys.mkdir path 0o777)

let copy_files (srcfiles : string list) (dstfiles : string list) : unit =
  List.iter
    (fun (src, dst) ->
      mkdirs (Filename.dirname dst);
      copy_file src dst)
    (List.combine srcfiles dstfiles)

let replace_prefix (before : string) (after : string) (fs : string list) :
    string list =
  let n = String.length before in
  List.map
    (fun f -> Filename.concat after (String.sub f n (String.length f - n)))
    fs

let alias_files (Args { srcdir; out = outdir; srcfiles } : args) : unit =
  copy_files srcfiles
    (replace_prefix srcdir (Filename.concat outdir "") srcfiles)

let alias_map (Args { lib; out = outfile; srcfiles } : args) : unit =
  let lib = String.capitalize_ascii lib in
  let modules =
    List.sort_uniq String.compare
      (List.rev_map
         (fun f ->
           String.capitalize_ascii
             (Filename.remove_extension (Filename.basename f)))
         srcfiles)
  in
  let content =
    Printf.sprintf "%s\n"
      (String.concat "\n"
         (List.map
            (fun m -> Printf.sprintf "module %s = %s__%s" m lib m)
            modules))
  in
  write_file outfile content

let usage : string =
  {|proto_macro_tool.opt -c <command> [<args>]

Commands:
  alias-files  Make aliased module files (e.g. copy 'a.ml',.. to 'mylib__A.ml',..).
  alias-map    Generate an alias map ('.mli' file) (e.g. write 'module A = Mylib__A',..).

Args:|}

let args : (string * Arg.spec * string) list =
  [
    ("-c", Arg.Set_string command, "Command");
    ("-l", Arg.Set_string lib, "Library");
    ("-s", Arg.Set_string srcdir, "Source root directory");
    ("-o", Arg.Set_string out, "Output file or directory");
  ]

let (_ : unit) =
  Arg.parse args (fun f -> srcfiles := f :: !srcfiles) usage;
  let args = read_args () in
  match !command with
  | "alias-files" -> alias_files args
  | "alias-map" -> alias_map args
  | _ -> Printf.printf "Unrecognized command: '%s'" !command
