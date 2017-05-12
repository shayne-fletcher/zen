let input_file_as_string (name : string) : string  =
  let ic = open_in_bin name in
  let len = 1024 in
  let s = Bytes.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input ic s 0 len in
      if n = 0 then()
      else
        begin
          Buffer.add_subbytes buf s 0 n;
          iter ()
        end
    with
      End_of_file -> ()
  in
  iter ();
  close_in ic;
  Buffer.contents buf

let default_style_options : string list =
  [ ".keyword { font-weight : bold ; color : Red }" ;
    ".keywordsign { color : #C04600 }" ;
    ".comment { color : Green }" ;
    ".constructor { color : Blue }" ;
    ".type { color : #5C6585 }" ;
    ".string { color : Maroon }" ;
    ".warning { color : Red ; font-weight : bold }" ;
    ".info { margin-left : 3em; margin-right: 3em }" ;
    ".param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }" ;
    ".code { color : #465F91 ; }" ;
    "pre { margin-bottom: 4px; font-family: monospace; }" ;
    "pre.verbatim, pre.codepre { }";
  ]

let print_code (buf : Buffer.t) (code : string) (source : string) : unit =
  Buffer.add_string buf "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n";
  Buffer.add_string buf "<html>\n";
  Buffer.add_string buf "<head>\n";
  Buffer.add_string buf "<style>\n";
  Buffer.add_string buf (String.concat "\n" default_style_options);
  Buffer.add_string buf "</style>\n";
  Buffer.add_string buf "<meta content=\"text/html; charset=iso-8859-1\" http-equiv=\"Content-Type\">\n";
  Buffer.add_string buf "<title>";
  Buffer.add_string buf source ;
  Buffer.add_string buf "</title>\n</head>\n";
  Buffer.add_string buf "<body>\n";
  Buffer.add_string buf "<pre>";
  Odoc_ocamlhtml.html_of_code buf ~with_pre:true code;
  Buffer.add_string buf "</pre>";
  Buffer.add_string buf "</body></html>"

let (version : bool ref) 
    ,(file : string ref) = ref false, ref ""

let read_args : unit -> unit  = fun () ->
  let specification =
    [("-v", Arg.Set version, "Print the version number") ;
     ("-f", Arg.String (fun s -> file := s), "File") ;
    ]
  in Arg.parse specification 
  (fun s -> 
    Printf.printf "Warning : Ignoring unrecognized argument \"%s\"\n" s) 
  ("Usage : "^(Sys.argv.(0))^" options")

let ()  = 
  read_args () ;
  if !version then print_string "0.0.0\n" 
  else
    if !file = "" then 
    raise (Failure 
      "Missing argument : file") ;
   if not (Sys.file_exists (!file)) then  
      raise (Failure (Printf.sprintf 
        "Bad argument : The file '%s' does not exist" !file)) ;
   try
     let code = input_file_as_string (!file) in
     let buf  = Buffer.create 1024 in
     print_code buf code (!file);
     print_string (Buffer.contents buf)
   with
   | _  as e -> Printf.printf "Exception : %s\n" (Printexc.to_string e)
