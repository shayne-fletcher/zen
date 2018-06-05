open! Core

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
    ".code { color : #465F91 ; background-color: #F5F5F5; }" ;
    "pre { margin-bottom: 4px; font-family: monospace; background-color: #F5F5F5; }" ;
    "pre.verbatim, pre.codepre { }";
  ]

let doctype =
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n"

let stylesheet =
  "<style>"^(String.concat ~sep:"\n" default_style_options)^"</style>\n"

let meta =
  "<meta content=\"text/html; charset=utf-8\" http-equiv=\"Content-Type\">\n"

let template source =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf doctype;
  Buffer.add_string buf "<html>\n";
  Buffer.add_string buf "<head>\n";
  Buffer.add_string buf stylesheet;
  Buffer.add_string buf meta;
  Buffer.add_string buf "<title>";
  Buffer.add_string buf source ;
  Buffer.add_string buf "</title>\n</head>\n";
  Buffer.add_string buf "<body>\n";
  Buffer.add_string buf "</body>\n";
  Buffer.add_string buf "</html>\n";
  Buffer.contents buf

let all = Highlight.all

