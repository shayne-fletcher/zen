open Printf

let get_fortune f g = 
  let repeat_eat_int ch n = 
    for i = 0 to (n - 1) do
      let _ = input_binary_int ch in
       ()
    done
  in 
    let bin_ch = open_in_bin g in
    try
      repeat_eat_int bin_ch 1;
      let num_quotes = input_binary_int bin_ch in
      repeat_eat_int bin_ch 3;
      Random.self_init ();
      seek_in bin_ch ((5 + (Random.int num_quotes))*4);
      let start_pos = input_binary_int bin_ch in
      let end_pos = input_binary_int bin_ch in
        close_in bin_ch;
        let n = (end_pos - start_pos - 2) in
  	let data_ch = open_in f in
           try
             seek_in data_ch start_pos;
             let s  = String.create n in
               really_input data_ch s 0 n;
               close_in data_ch;
               printf "\n%s\n" s
           with e ->
	     close_in_noerr data_ch;
	     raise e
    with e ->
      close_in_noerr bin_ch;
      raise e

let version = ref false
let filename = ref ""

let read_args () = 
  let specification = 
   [
    ("-v", Arg.Set version, "Print the version number");
    ("-f", Arg.Set_string filename, "Fortunes file") 
   ]
  in Arg.parse 
    specification 
    (fun s -> printf "warning : Ignoring unrecognized argument \"%s\"\n" s)  
    "Usage : fortune file"

let _ = 
  let invalid  = 
    fun x -> not ((Sys.file_exists x) && not (Sys.is_directory x))
  in
    read_args ();
    if !version then
      printf "1.0.0\n"
    else
      (
       let f = !filename in
       let g =  f^".bin" in
       match f with
         "" -> printf "error : Missing file argument\n"
       | _  when invalid f -> 
    	   printf "error : File \"%s\" does not exist or is a directory\n" f
       | _  when invalid g -> 
	   printf "error : File \"%s\" does not exist or is a directory\n" g
       | _ -> get_fortune f g
      )
