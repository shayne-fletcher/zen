open! Core
open! Soup.Infix

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

let template source =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n";
  Buffer.add_string buf "<html>\n";
  Buffer.add_string buf "<head>\n";
  Buffer.add_string buf "<style>\n";
  Buffer.add_string buf (String.concat ~sep:"\n" default_style_options);
  Buffer.add_string buf "</style>\n";
  Buffer.add_string buf "<meta content=\"text/html; charset=utf-8\" http-equiv=\"Content-Type\">\n";
  Buffer.add_string buf "<title>";
  Buffer.add_string buf source ;
  Buffer.add_string buf "</title>\n</head>\n";
  Buffer.add_string buf "<body>\n";
  Buffer.add_string buf "</body></html>";
  Buffer.contents buf

let highlight src =
  let soup = Soup.parse src in
  Highlight.code soup;
  Highlight.pre soup;
  Soup.to_string soup

