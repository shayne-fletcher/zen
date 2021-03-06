exception Out_of_bounds

module type STRING = sig

  type t
  type char

  val empty : t

  val length : t -> int
  val singleton : char -> t
  val append : t -> t -> t
  val get : t -> int -> char
  val sub : t -> int -> int -> t
  val iter_range : (char -> unit) -> t -> int -> int -> unit
  val print : Format.formatter -> t -> unit

end

module type ROPE = sig

  include STRING

  val set : t -> int -> char -> t
  val delete : t -> int -> t
  val insert_char : t -> int -> char -> t
  val insert : t -> int -> t -> t

  module Cursor : sig

    type cursor

    val create : t -> int -> cursor
    val position : cursor -> int
    val to_rope : cursor -> t
    val move_forward : cursor -> int -> cursor
    val move_backward : cursor -> int -> cursor
    val move : cursor -> int -> cursor
    val get : cursor -> char
    val set : cursor -> char -> cursor
    val insert_char : cursor -> char -> cursor
    val insert : cursor -> t -> cursor
    val print : Format.formatter -> cursor -> unit

  end

end

module type CONTROL = sig

  val small_length : int
  val maximal_height : int

end

module Make (S : STRING) (C : CONTROL) = struct

  type t =
  (*s, ofs, len with 0 <= ofs < len (s), ofs + len <= len (s)*)
  | Str of S.t * int * int
  (*t1, t2, len, height with 0 < len t1, len t2*)
  | App of t * t * int * int

  type char = S.char

  let empty : t = Str (S.empty, 0, 0)

  let length : t -> int = function 
    | Str (_, _, n) -> n
    | App (_, _, n, _) -> n

  let of_string (s : S.t) : t = Str (s, 0, S.length s)

  let singleton (c : char) : t = of_string (S.singleton c)

  let height : t -> int = function
    | Str _ -> 0
    | App (_, _, _, h) -> h

  let mk_app (t1 : t) (t2 : t) : t =
    App (t1, t2, length t1 + length t2, 1 + max (height t1) (height t2))

  let app : (t * t) -> t = function
    | Str (_, _, 0), t | t, Str (_, _, 0) -> t
    | Str (s1, ofs1, len1), Str (s2, ofs2, len2)
      when len1 <= C.small_length && len2 <= C.small_length ->
      Str (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2), 0, len1 + len2)
    | App (t1, Str (s1, ofs1, len1), _, _), Str (s2, ofs2, len2)
      when len1 <= C.small_length && len2 <= C.small_length ->
      App (t1,
           Str
             (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2),
              0, len1 + len2),
           length t1 + len1 + len2,
           1 + height t1)
    | Str (s1, ofs1, len1), App (Str (s2, ofs2, len2), t2, _, _)
      when len1 <= C.small_length && len2 <= C.small_length ->
      App (Str
             (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2),
              0, len1 + len2),
           t2,
           len1 + len2 + length t2,
           1 + height t2)
    | t1, t2 -> 
      App (t1, t2, length t1 + length t2, 1 + max (height t1) (height t2))

  let append (t1 : t) (t2 : t) : t = app (t1, t2)
  let ( ++ ) : t -> t -> t = append 

  let balance (t : t) : t =
    let rec to_list (((n, l) : int * t list) as acc) : t -> int * t list =
      function
        | Str _ as x -> n + 1, x :: l
        | App (t1, t2, _, _) -> to_list (to_list acc t2) t1 in
    let rec build (n : int) (l : t list) : t * t list =
      assert (n >= 1);
      if n = 1 then begin
        match l with
        | [] -> assert false
        | x :: r -> x, r
      end else
        let n' = n / 2 in
        let t1, l = build n' l in
        let t2, l = build (n - n') l in
        mk_app t1 t2, l in
    let n, l = to_list (0, []) t in
    let t, l = build n l in
    assert (l = []);
    t

  let rec unsafe_get (t : t) (i : int) : char = 
    match t with
    | Str (s, ofs, _) -> S.get s (ofs + i)
    | App (t1, t2, _, _) ->
      let n1 = length t1 in
      if i < n1 then unsafe_get t1 i else unsafe_get t2 (i - n1)

  let get (t : t) (i : int) : char =
    if i < 0 || i >= length t then raise Out_of_bounds;
    unsafe_get t i

  let rec mksub (start : int) (stop : int) (t : t) : t =
    (*Pre : 0 <= start < stop <= len t*)
    if start = 0 && stop = length t then t
    else match t with
    | Str (s, ofs, _) ->
      Str (s, ofs + start, stop - start)
    | App (t1, t2, _, _) ->
      let n1 = length t1 in
      if stop <= n1 then mksub start stop t1
      else if start >= n1 then mksub (start - n1) (stop - n1) t2
      else app (mksub start n1 t1, mksub 0 (stop - n1) t2)

  let sub (t : t) (ofs : int) (len : int) : t =
    let stop = ofs + len in
    if ofs < 0 || len < 0 || stop > length t then
      raise Out_of_bounds;
    if len = 0 then empty else mksub ofs stop t

  let rec safe_iter_range (f : char -> unit) (i : int) (n : int) : t -> unit = 
    function
    | Str (s, ofs, _) ->
      S.iter_range f s (ofs + i) n
    | App (t1, t2, _, _) ->
      let n1 = length t1 in
      if i + n <= n1 then
        safe_iter_range f i n t1
      else if i >= n1 then
        safe_iter_range f (i - n1) n t2
      else begin
        safe_iter_range f i n1 t1;
        safe_iter_range f (i - n1) (n - n1) t2
      end

  let iter_range (f : char -> unit) (t : t) (ofs : int) (len : int) : unit =
    if ofs < 0 || len < 0 || ofs + len > length t then
      raise Out_of_bounds;
    safe_iter_range f ofs len t

  let rec print (fmt : Format.formatter) : t -> unit  = function
    | Str (s, ofs, len) -> S.print fmt (S.sub s ofs len)
    | App (t1, t2, _, _) -> print fmt t1; print fmt t2

  let rec set_rec (i : int) (c : char) : t -> t = function
    | Str (s, ofs, len) when i = 0 ->
      app (singleton c, Str (s, ofs + 1, len - 1))
    | Str (s, ofs, len) when i = len - 1 ->
      app (Str (s, ofs, len - 1), singleton c)
    | Str (s, ofs, len) ->
      app (Str (s, ofs, i), app (singleton c, Str (s, ofs + i + 1, len - i - 1)))
    | App (t1, t2, _, _) ->
      let n1 = length t1 in
      if i < n1 then
        app (set_rec i c t1, t2)
      else
        app (t1, set_rec (i - n1) c t2)

  let set (t : t) (i : int) (c : char ) : t =
    let n = length t in
    if i < 0 || i >= n then raise Out_of_bounds;
    set_rec i c t

  let rec delete_rec (i : int) : t -> t = function
    | Str (_, _, 1) -> assert (i = 0); empty
    | Str (s, ofs, len) when i = 0 ->
      Str (s, ofs + 1, len - 1)
    | Str (s, ofs, len) when i = len - 1 ->
      Str (s, ofs, len - 1)
    | Str (s, ofs, len) ->
      app (Str (s, ofs, i), Str (s, ofs + i + 1, len - i - 1))
    | App (t1, t2, _, _) ->
      let n1 = length t1 in
      if i < n1 then
        app (delete_rec i t1, t2)
      else
        app (t1, delete_rec (i - n1) t2)

  let delete (t : t) (i : int) : t =
    let n = length t in
    if i < 0 || i >= n then raise Out_of_bounds;
    delete_rec i t

  let rec insert_rec (i : int) (r : t) : t -> t = function
    | Str _ as s when i = 0 ->
      app (r, s)
    | Str (_, _, len) as s when i = len ->
      app (s, r)
    | Str (s, ofs, len) ->
      Str (s, ofs, i) ++ r ++ Str (s, ofs + 1, len - i)
    | App (t1, t2, _, _) ->
      let n1 = length t1 in
      if i < n1 then
        app (insert_rec i r t1, t2)
      else
        app (t1, insert_rec (i - n1) r t2)

  let insert (t : t) (i : int) (r : t) : t =
    let n = length t in
    if i < 0 || i > n then raise Out_of_bounds;
    insert_rec i r t

  let insert_char (t : t) (i : int) (c : char) : t =
    insert t i (singleton c)

  module Cursor = struct
 
    type path =
      | Top
      | Left of path * t
      | Right of t * path

    type cursor = {
      rpos : int; (*position of the cursor relative to the current leaf*)
      lofs : int; (*offset {in chars} of the current leaf wrt the whole rope*)
      leaf : t; (*the leaf i.e. Str (s, ofs, len)*)
      path : path; (*context = zipper*)
    }

    let position (c : cursor) : int = c.lofs + c.rpos

    let rec unzip (t : t) : path -> t = function
      | Top -> t
      | Left (p, tr) -> unzip (app (t, tr)) p
      | Right (tl, p) -> unzip (app (tl, t)) p

    let to_rope (c : cursor) : t =
      unzip c.leaf c.path

    let create (r : t) (i : int) : cursor = 
      let rec zip (lofs : int) (p : path) : t -> cursor = function
        | Str (_, _, len) as leaf ->
          assert (lofs <= i && i <= lofs + len);
          { rpos = i - lofs; lofs = lofs; leaf = leaf; path = p}
        | App (t1, t2, _, _) ->
          let n1 = length t1 in
          if i < lofs + n1 then
            zip lofs (Left (p, t2)) t1
          else 
            zip (lofs + n1) (Right (t1, p)) t2 in
      if i < 0 || i > length r then raise Out_of_bounds;
      zip 0 Top r

    let get (c : cursor) : char =
      match c.leaf with
      | Str (s, ofs, len) ->
        let i = c.rpos in
        if i = len then raise Out_of_bounds;
        S.get s (ofs + i)
      | App _ -> assert false

    let set (c : cursor) (x : char) =
      match c.leaf with
      | Str (s, ofs, len) ->
        let i = c.rpos in
        if i = len then raise Out_of_bounds;
        let leaf = Str (S.singleton x, 0, 1) in
        if i = 0 then
          if len = 1 then
            { c with leaf = leaf }
          else
            { c with
              leaf = leaf;
              path = Left (c.path, Str (s, ofs + 1, len - 1)) }
        else if i = len - 1 then
          {
            lofs = c.lofs + len - 1;
            rpos = 0;
            leaf = leaf;
            path = Right (Str (s, ofs, len - 1), c.path)
          }
        else
          {
            lofs = c.lofs + i;
            rpos = 0;
            leaf = leaf;
            path = Left (Right (Str (s, ofs, i), c.path), 
                         Str (s, ofs + i + 1, len - i - 1))
          }
      | App _ -> assert false

    let rec concat_path (p1 : path) (p2 : path) : path = 
      match p1 with
      | Top -> p2
      | Left (p, r) -> Left (concat_path p p2, r)
      | Right (l, p) -> Right (l, concat_path p p2)

    let insert (c : cursor) (r : t) : cursor = 
      match c.leaf with
      | Str (s, ofs, len) ->
        let i = c.rpos in
        let cr = create r 0 in
        if i = 0 then
          {
            cr with
              lofs = c.lofs;
              path = concat_path cr.path (Left (c.path, c.leaf))
          }
        else if i = len then
          {
            cr with
              lofs = c.lofs + len;
              path = concat_path cr.path (Right (c.leaf, c.path))
          }
        else 
          {
            cr with
              lofs = c.lofs + i;
              path = concat_path cr.path 
                (Left (Right (Str (s, ofs, i), c.path),
                 Str (s, ofs + i, len - i)))
          }
      | App _ -> assert false

    let insert_char (c : cursor) (x : char) : cursor =
      insert c (of_string (S.singleton x))

    let next_leaf (c : cursor) : cursor =
      let lofs = c.lofs + length c.leaf in
      let rec down (p : path) : t -> cursor = function
        | Str _ as leaf -> { rpos = 0; lofs = lofs; leaf = leaf; path = p }
        | App (t1, t2, _, _) -> down (Left (p, t2)) t1
      in
      let rec up (t : t) : path -> cursor = function
        | Top -> raise Out_of_bounds
        | Right (l, p) -> up (mk_app l t) p
        | Left (p, r) -> down (Right (t, p)) r in
      up c.leaf c.path

    let rec move_forward_rec (c : cursor) (n : int) : cursor =
      match c.leaf with
      | Str (_, _, len) ->
        let rpos' = c.rpos + n in
        if rpos' < len then
          { c with rpos = rpos' }
        else if rpos' = len then begin
          try next_leaf c with Out_of_bounds -> { c with rpos = rpos' }
        end else (*rpos' > len*)
          let c = next_leaf c in
          move_forward_rec c (rpos' - len)
      | App _ -> assert false

    let move_forward (c : cursor) (n : int) : cursor =
      if n < 0 then invalid_arg "Rope.move_forward";
      if n = 0 then c else move_forward_rec c n

    let prev_leaf (c : cursor) : cursor  =
      let rec down (p : path) : t -> cursor = function
        | Str (_, _, len) as leaf ->
          {rpos = len; lofs = c.lofs - len; leaf = leaf; path = p}
        | App (t1, t2, _, _) -> down (Right (t1, p)) t2
      in
      let rec up (t : t) : path -> cursor = function
        | Top -> raise Out_of_bounds
        | Right (l, p) -> down (Left (p, t)) l
        | Left (p, r) -> up (mk_app t r) p
      in 
      up c.leaf c.path

    let rec move_backward_rec (c : cursor) (n : int) : cursor = 
      match c.leaf with
      | Str (_, _, len) ->
        let rpos' = c.rpos - n in
        if rpos' >= 0 then
          { c with rpos = rpos'}
        else (*rpos' < 0*)
          let c = prev_leaf c in
          move_backward_rec c (-rpos')
      |  App _ -> 
        assert false

    let move_backward (c : cursor) (n : int) : cursor =
      if n < 0 then invalid_arg "Rope.move_backward";
      if n = 0 then c else move_backward_rec c n

    let move (c : cursor) (n : int) : cursor =
      if n = 0 then c
      else if n > 0 then move_forward_rec c n
      else move_backward_rec c (-n)

    let rec leftmost (lofs : int) (p : path) : t -> cursor = function
      | Str _ as leaf -> { rpos = 0; lofs = lofs; leaf = leaf; path = p; }
      | App (t1, t2, _, _) -> leftmost lofs (Left (p, t2)) t1

    let delete (c : cursor) : cursor  = 
      match c.leaf with
      | Str (s, ofs, len) ->
        let i = c.rpos in
        if i = len then raise Out_of_bounds;
        if i = 0 then
          if len = 1 then 
            begin
              match c.path with
              | Top -> { c with leaf = empty }
              | Left (p, t) ->
                (*leftmost c.lofs p r*)
                let r = to_rope { c with leaf = t; path = p} in
                create r c.lofs
              | Right (t, p) ->
                let r = to_rope { c with leaf = t; path = p } in
                create r c.lofs
            end
          else
            {c with leaf = Str (s, ofs + 1, len - 1)}
        else if i = len - 1 then
          next_leaf { c with leaf = Str (s, ofs, len - 1) }
        else
          {
            lofs = c.lofs + i;
            rpos = 0;
            leaf = Str (s, ofs + i + 1, len - i -1);
            path = Right (Str (s, ofs, i), c.path)
          }
      | App _ -> assert false
        

  end

end
