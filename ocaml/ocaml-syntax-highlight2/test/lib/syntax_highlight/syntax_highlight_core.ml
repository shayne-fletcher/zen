open! Core

let%expect_test "template" =
  printf "%s" (Syntax_highlight.Syntax_highlight_core.template "foo");
  [%expect {|
    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
    <html>
    <head>
    <style>
    .keyword { font-weight : bold ; color : Red }
    .keywordsign { color : #C04600 }
    .comment { color : Green }
    .constructor { color : Blue }
    .type { color : #5C6585 }
    .string { color : Maroon }
    .warning { color : Red ; font-weight : bold }
    .info { margin-left : 3em; margin-right: 3em }
    .param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }
    .code { color : #465F91 ; }
    pre { margin-bottom: 4px; font-family: monospace; }
    pre.verbatim, pre.codepre { }</style>
    <meta content="text/html; charset=utf-8" http-equiv="Content-Type">
    <title>foo</title>
    </head>
    <body>
    </body></html> |}]
