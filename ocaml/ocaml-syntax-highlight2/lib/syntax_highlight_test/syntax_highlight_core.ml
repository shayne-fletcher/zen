open! Core
open! Syntax_highlight

let%expect_test "template" =
  printf "%s" @@ Syntax_highlight_core.template "foo" ;
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
    </body>
    </html> |}]

let%expect_test "all" =
  let src =
    {|
<html>
  <head>
    <title>Polynomials over rings</title>
  </head>
  <body>
    <h3>Arithmetics and rings</h3>
    <p>We begin with a type for modules implementing arithmetic.
      <pre>module type ARITH = sig
  type t
  val of_int : int -> t            val to_int : t -> int
  val of_string : string -> t      val to_string : t -> string
  val zero : t                     val one : t
  val add : t -> t -> t            val sub : t -> t -> t
  val mul : t -> t -> t            val div : t -> t -> t
  val compare : t -> t -> int      val equal : t -> t -> bool
end;;</pre>
    </p>
  </body>
</html>
|}
 in
   printf "%s" @@
   Syntax_highlight_core.doctype ^ Syntax_highlight_core.all src;
   [%expect {|
     <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
     <html><head>
         <title>Polynomials over rings</title>
       </head>
       <body>
         <h3>Arithmetics and rings</h3>
         <p>We begin with a type for modules implementing arithmetic.
           </p><pre><code class="code"><span class="keyword">module</span> <span class="keyword">type</span> <span class="constructor">ARITH</span> = <span class="keyword">sig</span>
       <span class="keyword">type</span> t
       <span class="keyword">val</span> of_int : int <span class="keywordsign">-&gt;</span> t            <span class="keyword">val</span> to_int : t <span class="keywordsign">-&gt;</span> int
       <span class="keyword">val</span> of_string : string <span class="keywordsign">-&gt;</span> t      <span class="keyword">val</span> to_string : t <span class="keywordsign">-&gt;</span> string
       <span class="keyword">val</span> zero : t                     <span class="keyword">val</span> one : t
       <span class="keyword">val</span> add : t <span class="keywordsign">-&gt;</span> t <span class="keywordsign">-&gt;</span> t            <span class="keyword">val</span> sub : t <span class="keywordsign">-&gt;</span> t <span class="keywordsign">-&gt;</span> t
       <span class="keyword">val</span> mul : t <span class="keywordsign">-&gt;</span> t <span class="keywordsign">-&gt;</span> t            <span class="keyword">val</span> div : t <span class="keywordsign">-&gt;</span> t <span class="keywordsign">-&gt;</span> t
       <span class="keyword">val</span> compare : t <span class="keywordsign">-&gt;</span> t <span class="keywordsign">-&gt;</span> int      <span class="keyword">val</span> equal : t <span class="keywordsign">-&gt;</span> t <span class="keywordsign">-&gt;</span> bool
     <span class="keyword">end</span>;;</code></pre>
         <p></p>


     </body></html> |}]

