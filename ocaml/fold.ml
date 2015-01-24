(* 
   The problem given is to express ``fold_left`` entirely in terms of
   ``fold_right``. For example, an attempted solution like

   ::

     let fold_left f e s =
     List.rev (fold_right (fun a acc -> f acc a) e (List.rev s))

   is inadmissiable because it relies on ``List.rev`` and thus not
   entirely in terms of ``fold_right``. So given that constraint, the
   solution needs to take the form
   ::

     let fold_left f e s =
     List.fold_right (*...something...*) s e

   Recall, given a function ``f``, a seed ``e_{0}`` and a list ``[a_{0};
   a_{1}; a_{2}; ...; a_{N}]``, ``fold_left`` computes

   ::

     f (... f ( f (f (f e_{0} a_{0}) a_{1}) ...) ...) A_{N}

   whereas ``fold_right`` computes
   ::

     f (... (f (f (f a_{N} e0) A_{N - 1}) a_{N-2})...) a_{0}

   So, the first observation is that when emulating ``fold_left`` with
   ``fold_right`` on encountering the term ``A_{k}``, we can't
   immediately apply ``f`` since we don't yet know the first argument
   to ``f`` as it depends on terms to the left of ``k`` and therefore
   we have to delay computation. The way to achieve that is to compute
   a function that can be invoked when the left argument becomes
   known. That function must somehow be the result of the 'update'
   step as we ``fold_right`` and at the point the solution starts
   writing itself
   ::

     let fold_left f e s =
     List.fold_right (fun a acc -> fun x -> acc (f x a)) s (fun x -> x) e

   The initial value for the 'accumulator' in the ``fold_right`` is
   the identity function and the result of the ``fold_right`` is a
   function in one argument and we apply it to the user provided
   'seed'.

   For example, in the top-level

   # fold_left (fun acc x -> x*x :: acc) [] [1; 2; 3;] ;;
   - : int list = [9; 4; 1]

   To see how this is working, consider the expression
   ::

     f (f (f e a_{0}) a_{1}) a_{2}

   that is, the result of a left fold over the list ``[a_{0}; a_{1};
   a_{2}] with function ``f`` and seed ``e``)

   On encountering ``a_{2}`` we produce ``fun x2 -> f x2 a2``
   On encountering ``a_{1}`` we produce ``fun x1 -> x2 (f x1 a1)``
   On encountering ``a_{0}`` we produce ``fun e -> x1 (f e a_{0})``

   To effect the ``fold_left`` we apply that last function to ``e`` and we're done.
*)

let fold_left f e s =
  List.fold_right (fun a acc -> fun x -> acc (f x a)) s (fun x -> x) e

(*
   As an aside, along the way I found out by accident that
   ``fold_right`` itself can be characterized this same way (as an
   accumulation of functions) by a slight re-arrangement
   ::

     let rec fold_right f s e = 
     (List.fold_right (fun a acc -> fun x -> f a (acc x)) s (fun x -> x)) e
*)

let rec fold_right f s e = 
  (List.fold_right (fun a acc -> fun b -> f a (acc b)) s (fun x -> x)) e
