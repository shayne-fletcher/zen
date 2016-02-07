let strfile (f : string) : unit =
  let rdstr (ic : in_channel) : string =
    let ls = [] in
    let rec loop ls =
      let l = input_line ic in
      match l with
      | "%" -> ls
      | _ -> loop (l :: ls) in
    String.concat "\n" @@ List.rev (loop ls) in

  let numstr = ref 0
  and longlen = ref 0
  and shortlen = ref 0 in
  
  let ic = open_in f in
  let oc = open_out_bin (f ^ ".dat") in
  seek_out oc (5 * 4); (*Skip over the header*)
  output_binary_int oc 0;
  try
    let rec loop () =
      let str = rdstr ic in
      let numchars  = String.length str in
      output_binary_int oc (pos_in ic);
      numstr := !numstr + 1; 
      longlen := max !longlen numchars; 
      shortlen := min !shortlen numchars;
      loop ()
    in loop ()
  with
  | End_of_file -> 
     close_in_noerr ic;
     seek_out oc 0;
     output_binary_int oc 1; (*version*)
     output_binary_int oc !numstr;
     output_binary_int oc !longlen;
     output_binary_int oc !shortlen;
     output_binary_int oc 0 (*flags*);
     close_out_noerr oc
  | Sys_error s -> 
     Printf.printf "Error : %s" s
     close_in_noerr ic;
     close_out_noerr oc;
  | x ->  
     Printf.printf "Error : Unexpected" s
     close_in_noerr ic;
     close_out_noerr oc;

let usage_msg : string = "Usage : strfile sourcefile"

let src : string ref = ref ""
let dst : string ref = ref ""
let version : bool ref = ref false

let read_args () =
   let specification =
    [("-v", Arg.Set version, "Print the version number"); ]
  in begin
    Arg.parse specification
      (fun s -> 
         if !src = "" then src := s 
         else dst := s)
      usage_msg;
  end

let () =
  read_args ();
  if !version then begin
    Printf.printf "1.0.0";
    ignore @@ exit 0
  end;
  let f = !src in
  match f with
  | "" -> Printf.printf "Missing argument 'sourcefile'"
  | _  when not ((fun x -> (Sys.file_exists x)&& not (Sys.is_directory x)) f)
         -> Printf.printf "File \"%s\" does not exist or is a directory\n" f
  | _ -> strfile f
       
