(** Root finding algorithms.

    A common problem encountered in engineering analysis is this : given
    a function {i f(x)}, determine the values of {i x} for which {i f(x)
    = 0}. The solutions (values of {i x}) are known as the roots of the
    equation {i f(x) = 0}, or the zeroes of the function {i f(x)}.

    These algorithms have been adapted from chapter 4. of "Numerical
    Methods in Engineering with Python" by Jaan Kiusalaas.
*)

(** Root finding implementation *)
include Roots_sig.S

