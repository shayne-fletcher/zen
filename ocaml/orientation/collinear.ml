(* Three points are collinear if the triangle contained by those three
   points has zero area.

  The area of a triangle is given by
  0.5 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)).
*)

type point = {x : int; y : int}

let collinear {x=x1; y=y1} {x=x2; y=y2} {x=x3; y=y3} =
  let a = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2) in
  a = 0
;;

(* 4 points in the plane will form a quadrilateral as long as not
   three of those points are collinear. *)

let powerset s =
  let rec powerset_rec s =
    match s with
    | [] -> [[]]
    | h :: tl ->
      let ys = powerset_rec tl in
      List.map (fun y -> h :: y) ys @ ys in
  powerset_rec s
;;

let quadilateral (ps : point list) =
  let f [p1;p2;p3] =
    not @@ collinear p1 p2 p3 in
  List.fold_left (fun acc x -> acc && x) true (
    List.map f (
      List.filter (fun l -> List.length l = 3) (powerset ps)
    )
  )
