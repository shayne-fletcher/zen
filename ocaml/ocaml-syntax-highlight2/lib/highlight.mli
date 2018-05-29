
val pre : Soup.soup Soup.node -> unit
(** [pre doc] replaces raw [<pre>] blocks in [doc] with [<pre><code
   class="code">] equivalents. *)

val code : Soup.soup Soup.node -> unit
(** [highlight_code doc] replaces raw code blocks in [doc] with [<code
    class="code">] equivalents. *)
