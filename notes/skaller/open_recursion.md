Skip to content
 
Search or jump to…

Pull requests
Issues
Marketplace
Explore
 @shayne-fletcher Sign out
5
1 0 solvuu-inc/solvuu-server Private
 Code  Issues 193  Pull requests 0  Projects 0  Wiki  Insights
Editing Translation to Open Recursion

Translation to Open Recursion
 
    Edit mode:  

# Step 1: Open Close recursive types

Given some recursive type which is self-recursive and not indirectly recursive,
such as
```
type expr = [
   | `Int of int
   | `Add of expr * expr
]
```
Change the name of the type by adding a quote at the end.
Add a type parameter to the type.
Now replace all recursions with the type paramemeter.
Finally re-close the type by defining the original name as the open type with itself as a parameter.
```
type 'e expr' = [
  | `Int of int
  | `Add of 'e * 'e
]
type expr = expr expr'
```
The open form of the type must be flat, removing all cycles, i.e. recursions.
The closure is obtained with an eta-expanded fixpoint operation.

Impact on existing code: NONE. The closed type has the same name as before and is identical to the previously defined type.

# Step 1a. Indirectly recursive types.
Consider two mutually recursive types.
To handle this case you need to add two types parameters and
proceed to create two open, flat types.
```
type addable =
  | `Int of int
  | `Add of subable * subable
  | `Inc of addable
and subable = 
  | `Float of float
  | `Sub of addable * addable
  | `Dec of subable
```
becomes
```
type ('a, 's) addable' = [
  | `Int of int
  | `Add of 's * 's
  | `Inc of 'a
type ('a, 's) subable' = [
  | `Float of float
  | `Sub of 'a * 'a 
  | `Dec of 's
type addable = (addable, subable) addable'
and subable = (addable, subable) subable'
```
It's important to use readable names for the type parameter, so perhaps
shortnames like those above are not so good in real code. You have to get
the order right too!

In a refactoring process one type can be converted at a time.
For example we could first open and close addable and leave subable alone.
However the process should be completed before extracting common subtypes.

# Step 2: Recursive Functions.
The same machinery is used for functions.
Given a function like
```
let rec eval (e:expr) : int = match e with
  | `Int i -> i
  | `Add (e1,e2) -> eval e1 + eval e2
```
We must first flatten the function removing recursions
by adding an extra parameter:
```
let eval' (f:expr -> int) (e:expr) = match e with
  | `Int i -> i
  | `Add (e1, e2) -> f e1 + f e2
```
Now we can close the open form the same way as with a type:
```
let rec eval x = 'eval eval x
```
OCaml the x is arbitrary but required due to the monomorphisation
restriction in Ocaml's type system. Another, way to do this is to
use the explicit fixpoint operator:
```
let rec fix f x = f (fix f) x
```
like so:
```
let expr x = fix expr' x
```

As for types, if you have indirect recursions, you need extra
parameters. 

There is also a small naming trick that may help:
```
let eval' (eval':expr -> int) (e:expr) = match e with
  | `Int i -> i
  | `Add (e1, e2) -> eval' e1 + eval' e2
```
Here I let the parameter name hide the function name.
Notice open forms are generally not recursive so you don't need the `rec`
and its a good idea to remove it.

Impact on existing code: NONE. The closed forms are identical to the
previously existing functions.

# Step 3: Opening and closing functions and type together
Generally, we use open functions on open types, not closed ones
as in the example. The open function on the open type looks much the
same as on the closed type, the only trick is to remember that the
argument type now has a type parameter. You may want to add it explicitly.

```
let eval' (eval':'e -> int) (e:'e expr') = match e with
  | `Int i -> i
  | `Add (e1, e2) -> eval' e1 + eval' e2
```
Note carefully the function's function parameter operates on the types type parameter!

When we use this method, we introduce the open form of the type and the
function then we may close both.

Impact on existing code: NONE.

# 4 Refactoring preparation
This step is invasive and technically not required but I strongly recommend it.
Polymorphic variants have universal constructors. The cannot be scoped, they
cannot be made private. Their meaning depends on the typing context in which
they're used, however Ocaml has type inference and that is complex enough
to follow without abusing the names of the constructors. If you have a constructor
used in one place such as the Ast, and in a different place such as a bound Term,
then the same name should be used if, and *only if* the argument of the constructor
is the same. 

For example, if you have lambda calculus you may have a form in which the lambda
constructor and variable constructor use string names. When you have nested lambda,
the same string name for the variable can be used, hiding the outer name. It is common
practice to alpha convert the terms replacing all the names with unique ones so
that there is no confusing hiding, and often, the strings are also replaced by
unique integers.

It is bad practice to use the same name for the lambda and variable terms here.
You should use say 
```
type slam = [`Slambda of string * slam | `Svar of string * slam]
type ilam = [`Ilambda of int * ilam | `Ivar of int * ilam]
```
instead. This makes the code more readable, and allows for easier
to read error message from the Ocaml type checker, especially in
the routine that converts one form to the other. It also ensures
a global data dictionary is possible.

Use the same polymorphic variant name if and only if it can be shared
between types: this usually means the types of the arguments are the
same but this is not quite the right description as we will say later.

# Step 5: Extract common subtypes from the open forms.
Often there exist a set of constructors which are logically related 
in some way and which are shared between types. Instead of declaring
the individual constructors each time, gather them into a single
type, and put them in a module alone. Now they can be shared,
by using the name of the type.

The syntax when defining the type is just the type name.
For example if you have:
```
type 't term1 = [
  | `Int of int | `Neg of 't 
  | `Add of 't * 't | `Sub of 't * 't
]
type 't term2 = [
  | `Int of int | `Neg of 't  
  | `Mul of 't * 't | `Div of 't * 't
]
```
you can rewrite it:
```
type 't base = [
  | `Int of int | `Neg of 't
]
type 't term1 = [
  | 't base
  | `Add of 't * 't | `Sub of 't * 't
]
type 't term2 = [
  | 't base
  | `Mul of 't * 't | `Div of 't * 't
]
```
Notice, critically, that the decomposition works best if the
variants are flat: we usually do this factoring on the open form 
of the types introduced in earlier steps. It can be done piecemeal.

Impact on existing code: NONE. We're just replacing a set of constructor
definitions with a single subtype name.

Warning: if you have two variants to combine each with one type parameter,
but the type parameters need to be closed to different recursions,
or just have different meanings, the combined variant needs two
type parameters, rather than one.

# Step 6: Extract common functions
Factoring functions is much the same as types. The factoring
should be on the open form of the functions and it should
follow the type factoring. Chosing *how* exactly to factor
and group terms is non-trivial however with polymorphic variants
in Ocaml you get a situation which is both good and bad: it doesn't matter!

Note carefully above that apart from the renaming of ambiguous constructors,
the transformation to open recursion has no impact on existing code.
This applies meta-recursively to the refactoring internally as well!
In other words, you can have more than one way of grouping things.
If everyone doesn't agree you may get some code duplication.

This flexibility is also a disadvantage: it means conflicts in
design cannot be detected by the type system. This is the same
problem as the use of the same polymorphic constructor name
for different purposes: the typing context will disambiguate the use
and the conflict will only appear when you make an error or try
to combine types using the same constructor name for different
purposes.

When designing with open recursion, developers can and should first
focus on the open forms to get the details of the operations and
types correct. The actual design process consists of picking
the closure points.

# Covariant Extension
You may wonder why bother to do any of this stuff. Factoring
obviously allows for modularity. It allows tasks to be delegated
more easily.

However there is a very important property of the above technique
which makes the method essential if you want sharing.  Consider
the type
```
type expr = [
  | `Int of int
  | `Add of expr * expr
]
let rec eval (e:expr) = match e with
  | `Int i -> i
  | `Add (e1, e2) -> eval e1 + e2
```

Suppose we want to allow subtraction as well:

```
type expr = [
  | `Int of int
  | `Add of expr * expr
  | `Sub of expr * expr
]
let rec eval (e:expr) = match e with
  | `Int i -> i
  | `Add (e1, e2) -> eval e1 + e2
  | `Sub (e1, e2) -> eval e1 - eval e2
```

Now, you have to replace addable everywhere with the new type.
All the functions have to be rewritten to handle the new term.
If you use open recursion instead lets see what happens:
```
type 'a expr0' = [
  | `Int of int
  | `Add of 'a * 'a
]
let rec eval0' (eval0':'a -> int) (e:'a expr0') = match e with
  | `Int i -> i
  | `Add (e1, e2) -> eval0' e1 + eval0' e2
type expr0 = expr0 expr0'
let eval0 x = fix eval0' x
```
and then
```
type 'a expr1' = [
  | #expr0'
  | `Sub of 'a * 'a
let rec eval1' (eval1':'a -> int)  (e:'a expr1') = match e with
  | #expr0' as x -> (expr0' eval1' x :> 'a expr1')
  | `Sub (e1, e2) -> eval1' e1 - eval1' e2
type expr1 = expr1 expr1'
let eval1 x = fix eval1 x
```

Notice the first functions and types in the 0'th level are not modified at all.
The extension is a pure extension. But what is not evident here is that
the closed type `expr0` is a *subtype* of `expr1`. In particular you can
pass an expr0 to eval1 OR to eval0.

It is not just the constructor list of the type which is extended but
the type of the constructor argument as well.

In other words the version 1 API will accept values of the version 0 API
as well as the values introduced in the new API. The new API is a pure
extension of the old one. 

When you add new constructors, this is known as width subtyping
(the old type is a subtype of the new one because it has a subset
of the constructors). However, when the arguments of the constructors
also get extended this is known as depth subtyping. For variant types,
the extension is covariant, meaning when you add constructors to get
a supertype, the arguments extend as well.

The argument of a function, on the other hand is contravariant whilst
the return type is covariant. This means you have to dynamically check
you actually have one of the subtype constructors before calling
the subtype function, and this means in depth as well, meaning,
recursively down the tree. You cannot call eval0 on a term just because
it is an Add term, because the arguments might include Sub terms.

How do you do this check? With a pattern match!
Ocaml handles the variance automatically.

It does this because the depth subtyping is *removed* from the open form.
The open forms are flat, they only have width subtyping. The depth is
introduced by the closure (fixation) operations. Its sometimes called
"closing the recursive knot".

# Subtyping Lattice
With the open recursion technique you can *partition* all the constructors
into sets and make them types. These are base types which may be considered
extensions of the non-existent empty type, the root.

Then, just using basic theory of finite sets, you can combine these types
into supertypes, recursively. Duplication is allowed and ignored. This
results in a standard subset inclusion lattice.

Any of these types can be closed, if you close all of them an isomorphic 
subtyping lattice is formed. In other words if the open type A has a subset
of the constructors of B, then A is a subtype of B, and, the closure of A
is also a subtype of B.

Therefore, a value of a more basic type can always be passed to a function
accepting as an argument any supertype, and in particular, values of the 
basic system can always be passed to functions designed to handle extensions.

This is because such functions extract the subtypes with a pattern match
and dispatch to the basic function that would have handled them anyhow.
This technique is not enforced, but it must work.

# Caveats
In order for covariant extension of open recursive types to work, the
variant arguments must be covariant. Products (tuples, records) and sums 
(variants of the ordinary or polymorphic kind) are covariant. Mutable
fields however are *invariant* and functions are *contravariant* in their
domain and covariant in their codomain.

This does not mean functions or mutable fields cannot be part of the
constructor argument type: it means that if they are recursion parameter
cannot go through the non-convariant part.





Edit Message

Write a small message here explaining this change. (Optional)
© 2018 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
API
Training
Shop
Blog
About
Press h to open a hovercard with more details.