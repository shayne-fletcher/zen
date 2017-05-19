open Ml_types

let generic_level = 100000000

let new_id = ref (-1)

let newty2 level desc =
  incr new_id; {desc; level; id = !new_id }

let newgenty desc = newty2 generic_level desc

let newgenvar ?name () = newgenty (Tvar name)
