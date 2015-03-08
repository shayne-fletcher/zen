(*main.ml*)

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      Rpcalc.input Lexer.token lexbuf
    done
  with
  | End_of_file -> exit 0

let () = Printexc.print main ()

(*
$ rpcalc
4 9 +
	13
3 7 + 3 4 5 *+-
	-13
3 7 + 3 4 5 * + - n	%Note the unary minus, n
	13
5 6 / 4 n +
	-3.166666667
3 4 ^			%Exponentiation
	81
^D			%End-of-file indicator
$
 *)
