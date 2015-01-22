(*Module signature*)
module type E_sig = sig type t end
(*Physical realization of that signature*)
module M : E_sig with type t = int = struct type t = int end
(*'f' is a function that returns a "first class module"*)
let f : unit -> (module E_sig with type t = int) = fun () -> (module M : E_sig with type t = int)
(*"unpack" the result of calling 'f'*)
module M' = (val (f ()) : E_sig with type t = int)
