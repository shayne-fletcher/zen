let caml_echo input = input

let _ = Callback.register "caml_echo" caml_echo
