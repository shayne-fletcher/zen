let read_file (filename : string) : string =
  In_channel.with_open_bin filename (fun input -> In_channel.input_all input)

let write_file (filename : string) (contents : string) : unit =
  Out_channel.with_open_bin filename (fun output ->
      Out_channel.output_string output contents)

let copy_file (src : string) (dst : string) : unit =
  write_file dst (read_file src)

let rec mkdirs (pre : string) (path : string) : unit =
  if Filename.concat path "" <> pre then mkdirs pre (Filename.dirname path);
  if not (Sys.file_exists path) then Sys.mkdir path 0o777

let copy_files (out_dir : string) (srcfiles : string list)
    (dstfiles : string list) : unit =
  List.iter
    (fun (src, dst) ->
      mkdirs out_dir (Filename.dirname dst);
      copy_file src dst)
    (List.combine srcfiles dstfiles)

let replace_prefix (old_prefix : string) (new_prefix : string)
    (files : string list) : string list =
  let n = String.length old_prefix in
  List.map
    (fun f -> Filename.concat new_prefix (String.sub f n (String.length f - n)))
    files

let really_alias_files
    ((lib, srcdir, outdir, srcfiles) : string * string * string * string list) :
    unit =
  copy_files outdir srcfiles (replace_prefix srcdir outdir srcfiles)

let really_alias_map
    ((lib, _srcdir, out, srcfiles) : string * string * string * string list) :
    unit =
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
  write_file out content

let command = ref ""
let lib = ref ""
let out = ref ""
let srcdir = ref ""
let srcfiles = ref []

let ensure_trailing_slash (s : string) : string =
  if not (String.ends_with ~suffix:"/" s) then Filename.concat s "" else s

let alias_files () : unit =
  let lib = !lib in
  let srcdir = ensure_trailing_slash !srcdir in
  let out = ensure_trailing_slash !out in
  let srcfiles =
    List.filter
      (fun f -> Sys.file_exists f && String.starts_with ~prefix:srcdir f)
      !srcfiles
  in
  really_alias_files (lib, srcdir, out, srcfiles)

let alias_map () : unit =
  let lib = !lib in
  let srcdir = ensure_trailing_slash !srcdir in
  let out = !out in
  let srcfiles =
    List.filter
      (fun f -> Sys.file_exists f && String.starts_with ~prefix:srcdir f)
      !srcfiles
  in
  really_alias_map (lib, srcdir, out, srcfiles)

let go () : unit =
  match !command with
  | "alias-files" -> alias_files ()
  | "alias-map" -> alias_map ()
  | cmd -> Printf.printf "Unrecognized command: '%s'" cmd

let usage : string =
  {|proto_macro_tool.opt -c <command> [<args>]

   Commands:
   alias-files  Make aliased module files (e.g. copy 'a.ml',.. to 'mylib__A.ml',..).
   alias-map    Generate an alias map ('.mli' file) (e.g. write 'module A = Mylib__A',..).

   Args:|}

let args : (string * Arg.spec * string) list =
  [
    ("-c", Arg.Set_string command, "Command");
    ("-l", Arg.Set_string lib, "Library name");
    ("-s", Arg.Set_string srcdir, "Source root directory");
    ("-o", Arg.Set_string out, "Output file or directory name");
  ]

let files (filename : string) : unit = srcfiles := filename :: !srcfiles

let (_ : unit) =
  Arg.parse args files usage;
  go ()
