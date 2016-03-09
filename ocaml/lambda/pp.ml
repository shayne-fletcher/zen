type t =
| Block of t list * int * int (*es, indent, length*)
| String of string
| Break of int

let rec break_dist : t list * int -> int = function
  | (Block (_, _, len) :: es, after) -> len + break_dist (es, after)
  | (String s :: es, after) -> String.length s + break_dist (es, after)
  | (Break _ :: es, after) -> 0
  | ([], after) -> after

let length : t -> int = function
  | (Block (_, _, len)) -> len
  | (String s) -> String.length s
  | (Break len) -> len

let string (s : string) : t= String s

let break (len : int) : t = Break len

let block ((indent : int), (es : t list)) : t =
  let rec sum = function |
      ([], k) -> k | (e :: es, k) -> sum (es, length e + k) in
  Block (es, indent, sum (es, 0))

module Detail = struct
(*Return [s] padded on the left with i - |s| of the character [c]*)
let pad_left (c : char) (i : int) (s : string) : string =
  let l = i -  (String.length s) in
  if l <= 0 then s
  else (String.make l c) ^ s
end

let print ((oc : out_channel), (e : t), (margin : int)) : unit =
  let space = ref margin in
  let blanks (n : int) : unit = 
    output_string oc (Detail.pad_left ' ' n "");
    space := !space - n in
  let newline () : unit = 
    output_char oc '\n'; space := margin in
  let rec printing = 
      function
      | ([], _, _) -> ()
      | (e :: es, blockspace, after) ->
          begin
          match e with
          | Block (bes, indent, len) -> 
              printing (bes, !space - indent, break_dist (es, after))
          | String s -> 
              output_string oc s; space := !space - String.length s
          | Break len ->
              if len + break_dist (es, after) <= !space
            then blanks len
            else (newline (); blanks (margin - blockspace))
        end;
        printing (es, blockspace, after)
        in
  printing ([e], margin, 0)
        
    
