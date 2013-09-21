(*e.g.
  Match files in directory with given suffix and invoke a program on
  them.

$ ./build/bin/filesin-win-mlfi-402-x86_64.exe -d \
    c:/project/mlfi/libs/qfd_lang/examples -p \
    c:/project/mlfi/build/bin/qfd_qfd_lang_repl_win-win-mlfi-402-x86_64.exe
*)

(*Globals*)
let version,root,prog,suffix=ref false,ref "",ref "",ref ""

let (read_args:unit -> unit) = fun () ->
  let specification =
    [("-v", Arg.Set version, "Print the version number") ;
     ("-d", Arg.String (fun s -> root := s), "Directory") ;
     ("-s", Arg.String (fun s -> suffix := s), "Suffix") ;
     ("-p", Arg.String (fun s -> prog := s), "Path to the prog") ;
    ]
  in Arg.parse specification
  (fun s -> Printf.printf "Warning : Ignoring unrecognized argument \"%s\"\n" s)
  ("Usage : "^(Sys.argv.(0))^" options")

let (filesin:string -> string array option) = fun s ->
  if not (Sys.file_exists s)
       || not (Sys.is_directory s) then
    None
  else (Some (Sys.readdir s))

let (read_and_close:Unix.file_descr -> string)  = fun fdr ->
  let buf = Buffer.create 1024 in
  let bytes_to_read = ref true in
  while !bytes_to_read do
    let s  = String.create 1024 in
    let num_bytes_read = Unix.read fdr s 0 124 in
    Buffer.add_string buf (String.sub s 0 num_bytes_read) ;
    if num_bytes_read < 1024 then bytes_to_read := false ;
    ()
  done ;
  Unix.close fdr ;
  Buffer.contents buf

let (process_file:string -> unit) = fun f ->
  let p = Filename.concat (!root) f in
  let cmd = Printf.sprintf "%s -l -e %s" (!prog) p in
  (*I tried to use anonymous pipes here but that seems broken on
    Windows when [Unix.system] is involved*)
  let tf=Filename.temp_file "tmp" "" in
  let fdw = Unix.openfile tf [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT] 0o666 in
  let tmp = Unix.dup Unix.stdout in
  let stat = (
    Unix.dup2 fdw Unix.stdout ;
    Printf.printf "\n*** - %s:\n" p; flush stdout;
    Unix.system cmd  (*Invoke the intpreter on f*)
  )
  in
  Unix.dup2 tmp Unix.stdout ; Unix.close tmp; Unix.close fdw;
  match stat with
  | Unix.WEXITED 127 ->
    Printf.printf "The command \"%s\" could not be executed" cmd
  | _ ->
    let fdr = Unix.openfile tf [Unix.O_RDONLY] 0o666 in
    Printf.printf "%s" (read_and_close fdr) ; flush stdout (*; Sys.remove tf*)

let (process_files:string list -> unit) = fun fs ->
  List.iter (fun f -> process_file f) fs

let (main:unit) =
  read_args () ; Printf.printf "%s" !prog ;
  if !version then print_string "0.0.0\n"
  else
    if !suffix = "" then
    raise (Failure
      "Missing argument : suffix") ;
   if not (Sys.file_exists (!prog)) then
      raise (Failure (Printf.sprintf
        "Bad argument : The file '%s' does not exist" !prog)) ;
   try
     match (filesin !root) with
     | Some names ->
       let n=String.length !suffix in
       let pred e =
         let i = (String.length e - n) in
         (i >= 0) && (String.sub e i n) = !suffix
       in process_files (List.filter pred (Array.to_list names) )
     | None -> ()
   with
   | _  as e -> Printf.printf "Exception : %s\n" (Printexc.to_string e)
