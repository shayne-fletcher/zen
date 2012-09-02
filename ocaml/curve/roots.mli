(** A common problem encountered in engineering analysis is this :
  given a function {i f(x)}, determine the values of {i x} for which
  {i f(x) = 0}. The solutions (values of {i x}) are known as the
  roots of the equation {i f(x) = 0}, or the zeroes of the function
  {i f(x)}.

  These algorithms have been adapted from chapter 4. of "Numerical
  Methods in Engineering with Python" by Jaan Kiusalaas.
*)
module Roots :
  sig

    (** {2 Search algorithms}*)

    (** Search for a zero of {i f(x)} in the interval {i (a, b)} in
        increments of [dx]. It returns the bounds [Some (x1, x2)] if
        the search was successful and [None] if not. After the first
        root (the root closest to {i a}) has been detected, [search]
        can be called again with {i a} replaced by [x2] in order to
        find the next root. This can be repeated as long as [search]
        detects a root.

        @example [let f x = x**3. -. 10.*.(x**2.)  +. 5. in search f
        0. 1. 0.2] returns Some (0.60000000000000009, 0.8)
    *)
    val search : (float -> float) -> float -> float -> float -> (float * float) option

    (** {2 Root finders} *)

    (** Use the method of bisection to compute the root of {i f(x) = 0
        } that is known to lie in the interval [(x1, x2)]. The number
        of bisections {i n} required to reduce the interval to {i eps}
        is computed.
        @param f The function {i f(x)}
        @param bounds The interval known to contain the root
        @param eps The length of the interval in which to constrain the root
    *)
    val bisect : (float -> float) -> float * float -> float -> float

    (** This Newton-Raphson method assumes that the root to be
        computed is initially bracketed in {i [a, b]}. The midpoint of
        the bracket is used as the initial guess of the root. The
        brackets are updated after each iteration. If a Newton-Raphson
        step jumps brackets, it is replaced with a bisection step.
        @param f The function {i f(x)}
        @param f' The derivative of {i f(x)}
        @param bounds The interval known to contain the root
        @param tol Tolerance
        @param max_its Maximum number iterations permitted
    *)
    val newton : (float -> float) -> (float -> float) -> float * float -> float -> int -> float

  end
;;
