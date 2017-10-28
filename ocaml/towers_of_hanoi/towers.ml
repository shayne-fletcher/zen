#require "core";;
open Printf;;

(* Towers of Hanoi -- 'Algorithms and Data Structures', Kingston, 2nd
   ed. 1998. *)
let rec towers n from to_ spare =
  if n > 0 then
    begin
      towers (n - 1) from spare to_;
      printf  "Move the top disk from peg %c to peg %c\n" from to_;
      towers (n - 1) spare to_ from
    end
else
  ()
;;

let () = towers 3 'a' 'c' 'b';;
