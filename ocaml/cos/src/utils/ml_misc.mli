val fatal_error: string -> 'a
val fatal_errorf: ('a, Format.formatter, unit, 'b) format4 -> 'a

exception Fatal_error
