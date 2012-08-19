open Printf

let num_quotes = ref 0
let longest = ref 0
let shortest = ref max_int

let header out_ch = 
  output_binary_int out_ch 1; (*Version.*)
  output_binary_int out_ch !num_quotes; (*Number of quotations.*)
  output_binary_int out_ch !longest; (*Longest quote length.*)
  output_binary_int out_ch !shortest; (*Shortest quote length.*)
  output_binary_int out_ch 0 (*Flags; these get set to zero.*)

let read_quote in_ch =
  let quote = ref "" in
    let rec loop () =
      let line = input_line in_ch in
        match line with
          "%" -> !quote 
        | _ -> quote := !quote ^ line ^ "\n"; loop ()
    in
      loop()

let process f =
  let in_ch = open_in f in
  let out_ch = open_out_bin (f ^ ".bin") in
  seek_out out_ch (5*4); (*Skip over the header.*)
  try
    printf "%d\n" 0;
    output_binary_int out_ch 0;
    
    let rec loop () = 
      let quote = read_quote in_ch in
        (*printf "%s\n" quote;*)
        (* printf "%d\n" (pos_in in_ch); *)
        output_binary_int out_ch (pos_in in_ch);
        shortest := min !shortest (String.length quote);
        longest := max !longest (String.length quote);
        num_quotes := !num_quotes + 1;
        loop ()
    in
      loop ()
  with e -> 
    close_in_noerr in_ch;
    seek_out out_ch 0;
    header out_ch;
    close_out_noerr out_ch

let version = ref false
let filename = ref ""

let handle_version () = printf "1.0.0\n"

let handle_unexpected_arg = 
  fun s -> printf "warning : Ignoring unrecognized argument \"%s\"\n" s

let read_args () = 
  let specification = 
   [
    ("-v", Arg.Set version, "Print the version number");
    ("-f", Arg.Set_string filename, "Fortunes file") 
   ]
  in let usage = "Usage : strfile file"
     in Arg.parse specification handle_unexpected_arg  usage

let _ = 
  read_args ();
  if !version then
    handle_version ()
  else
  (
    let f = !filename in
      match f with
        "" -> printf "error : Missing file argument\n"
      | _  when not ((fun x -> (Sys.file_exists x)&& not (Sys.is_directory x)) f)
           -> printf "error : File \"%s\" does not exist or is a directory\n" f
      | _ -> process f
  )
