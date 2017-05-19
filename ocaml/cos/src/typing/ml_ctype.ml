let current_level = ref 0

let newty2 = Ml_btype.newty2
let newty desc = newty2 !current_level desc
let newvar ?name () = newty2 !current_level (Tvar name)
