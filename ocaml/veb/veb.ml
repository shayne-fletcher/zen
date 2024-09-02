type node = Leaf |
  Node of {
      parent: node option;
      left: node;
      right: node;
      mark: bool;
    }

make (h: int) : node
make h = make_rec (h + 1) Nothing
  where
    make_rec :: Int -> Maybe Node -> Node
    make_rec level p =
      case level of
        0 -> Nil
        _ ->
          let n =
                Node {
                  parent = p,
                  left = make_rec (level - 1) (Just n),
                  right = make_rec (level - 1) (Just n),
                  marked = False
                }
          in n

let rec t =
  Node {
      parent = None;
      left = begin
          let rec ln =
            Node { parent = Some t;
                   left = begin
                       let lln = Node { parent = Some ln; left = Leaf; right = Leaf; mark =  false}
                       in lln
                       end;
                   right= begin
                       let rln = Node { parent = Some ln; left = Leaf; right = Leaf; mark = false}
                       in rln
                       end;
                   mark = false;
              }
          in ln
        end;
      right = begin
              let rec rn =
                Node { parent = Some t;
                       left = begin
                           let lrn = Node { parent =  Some rn; left = Leaf; right = Leaf; mark = false}
                           in lrn
                           end;
                       right = begin
                           let rrn = Node { parent = Some rn; left = Leaf; right = Leaf; mark = false}
                           in rrn
                           end;
                       mark = false;
                  }
              in rn
        end;
n      mark = false
    }

let rec fold f acc t =
  match t with
  | Node { left = l; right = r } as n -> f (fold f (fold f acc l) r) n
  | Leaf as n -> f acc n
