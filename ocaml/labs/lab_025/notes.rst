==============
 Huet Zippers
==============

Given a recursive algebraic datatype [T('a)] , there is associated with it a derivative type [T'('a)] called it's "zipper". This type is convenient when writing programs that traverse [T] values arbitraily and update their contents, especially in functional programming languages.

Consider the ['a list] datatype for example. It is easy to write a function that updates a list in a purely functional way:
```
let rec list_set (l : 'a list) (i : int) (v : 'a) : 'a list =
  assert (i >= 0);
  match l with
  | [] -> failwith "list_set"
  | (_ :: tl) when i = 0 -> v :: tl
  | (h :: tl) -> h :: list_set tl (i - 1) v

```
The trouble with this function though is that it is wasteful of memory in that it must constantly allocate new list nodes. In the presence of multiple updates in a specific region of the list, it also wastes time since it constantly has to crawl to find the right position in the list, then crawl back creating new nodes. If the intent is to make several close-by changes, this is very wasteful. It would be better if we could crawl forward along the list to a certain point then make multiple updates before crawling back and allocating new nodes. This is what a list zipper allows us to do.

A list zipper can be represented as a pair of lists. 
```  
type 'a t = Z of 'a list * 'a list
```
The type encapsulates the notion of a cursor. The cursor is thought of as being at a specific position in the list. Nodes to the left are stored in the first field of the pair (in reverse order), nodes to the right in the second. Using this model we can implement moving forward or backward through the list and updating the value at the cursor. Multiple operations to the same area of the list only require a small bit of crawling and relatively few allocations. In fact, every individual operation takes constant time and constant allocations. As long as you don't need to move the cursor much, there is a significant gain over the naive method of updating lists.

Here's a complete implemementation.
```
  type 'a t = Z of 'a list * 'a list

  let empty : 'a t = Z ([], [])

  let from_list : 'a list -> 'a t = 
    fun l -> Z ([], l)

  let to_list : 'a t -> 'a list = 
    function | Z (ls, rs) -> List.rev ls @ rs

  let go_right : 'a t -> 'a t = function
    | Z (ls, h :: tl) -> Z (h :: ls, tl)
    | z -> z

  let go_left : 'a t -> 'a t = function
    | Z (h :: tl, rs) -> Z (tl, h :: rs)
    | z -> z

  let insert (a : 'a) : 'a t -> 'a t = function
    | Z (ls, rs) -> Z (ls, a :: rs)

  let delete : 'a t -> 'a t = function
    | Z (ls, _ :: rs) -> Z (ls, rs)
    | z -> z

  let replace (l : 'a t) (a : 'a) : 'a t =
    match l with
    | Z (ls, _ :: rs)  -> Z (ls, a :: rs)
    | z -> z

```

Here's a type definition for a binary search tree. 
```
type 'a tree = E | N of 'a tree * 'a * 'a tree
``

The zipper for a binary search tree is in effect a list of trees.
```
type 'a path =
  | Root
  | Left of 'a * 'a tree * 'a path
  | Right of 'a tree * 'a * 'a path

type 'a cursor = 'a tree * 'a path
```
The cursor in this case points to the sub-tree of interest (the focus) and the path through the tree to get to that point. The path represents the context of the sub-tree of interest.
```
exception Top
exception Bottom

let make_tree : 'a tree * 'a * 'a tree -> 'a tree = 
  fun (l, v, r) -> N (l, v, r)

let move_left : 'a cursor -> 'a cursor = fun (tree, path) ->
  match tree with
  | E -> raise Bottom
  | N (l, v, r) -> (l, Left (v, r, path))

let move_right : 'a cursor -> 'a cursor = fun (tree, path) ->
  match tree with
  | E -> raise Bottom
  | N (l, v, r) -> (r, Right (l, v, path))

let move_up : 'a cursor -> 'a cursor = fun (tree, path) ->
  match path with
  | Root -> raise Top
  | Left (v, r, tail) -> (make_tree (tree, v, r), tail)
  | Right (l, v, tail) -> (make_tree (l, v, tree), tail)

let of_tree : 'a tree -> 'a cursor = function
  | E -> failwith "of_tree"
  | N (l, v, r) as n -> (n, Root)

let rec to_tree : 'a cursor -> 'a tree = function
  | (t, Root) -> t
  | (l, Left (v, r, path)) ->
    to_tree (make_tree (l, v, r), path)
  | (r, Right (l, v, path)) ->
    to_tree (make_tree (l, v, r), path)

let replace : 'a cursor -> 'a tree -> 'a cursor =
  fun c t -> 
    match c with
    | _, Root -> (t, Root)
    | (_, Left (v, r, path)) -> (t, Left (v, r, path))
    | (_, Right (l, v, path)) ->  (t, Right (l, v, path))

let delete : 'a cursor -> 'a cursor =  fun c -> replace c E
```
In terms of the above data-type, write the following functions
```
  val most_left : 'a tree -> 'a cursor
  val most_right : 'a tree -> 'a cursor
  val first_leaf : 'a tree -> 'a cursor
  val next_leaf : 'a cursor -> 'a cursor
  val collect_leaves : 'a tree -> 'a list
```
[most_left t]` computes the left-most node in a given binary tree, `most_right t` the right-most. For example, given the tree
```
       1
     2
   3
```
[most_left] returns a cursor pointing at the node labeled '3', [most_right] a cursor pointing at the node labeled '1' whereas, given the tree
```
       1
         2
           3
```
[most_left] finds the node labeled '1' and [most_right] the node labeled '3'.

`[first_leaf t]` finds the first leaf node in [t] in a left-to-right sense. For example, the first leaf in 
```
       1
     2   3
   4
```
is the node labeled '4'.

`[next_leaf c]` finds the "next" leaf from the leaf represented by the cursor `[c]` for example, in
```
       1
     2   3
   4   5
```
[next (first_leaf t)] computes the node labeled '5'.

Finally, [collect_leaves] finds all of the leaf node values in order. For example, given the above tree, [collect_leavs t] is the list [[4; 5; 3]].
