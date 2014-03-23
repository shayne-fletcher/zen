let sum t = 
(*http://en.wikipedia.org/wiki/Sum

  Summation is the operation of adding a sequence of numbers

 *)
List.fold_left (fun acc x -> acc +. x) 0.0 t

let arithmetic_mean t = 
  (*http://en.wikipedia.org/wiki/Arithmetic_mean

    Simply the mean or average when the context is clear, is the sum
    of a collection of numbers divided by the number of numbers in the
    collection.

   *)
(sum t)/.(float_of_int (List.length t))

let harmonic_mean t =
  (*http://en.wikipedia.org/wiki/Harmonic_mean

    The harmonic mean is the preferable method for averaging
    multiples, such as the price/earning ratio, in which price is in
    the numerator. If these ratios are averaged using an arithmetic
    mean (a common error), high data points are given greater weights
    than low data points. The harmonic mean, on the other hand, gives
    equal weight to each data point.

   *)

 let n = float_of_int (List.length t) in
 let denom=List.fold_left 
             (fun acc xi -> acc +. 1.0 /. xi) 0.0 t 
 in n /. denom

let standard_deviation t =
  (*http://en.wikipedia.org/wiki/Standard_deviation

    For a finite set of numbers, the standard deviation is found by
    taking the square root of the average of the squared differences
    of the values from their average value.

   *)
 let av = arithmetic_mean t in
 let squared_diffs = List.fold_left (fun acc xi -> ((xi -. av) *. (xi -. av)) :: acc) [] t
 in sqrt (arithmetic_mean squared_diffs)

let _ = Printf.printf "%f\n" (12.0/.7.0); Printf.printf "%f\n" (harmonic_mean [1.0;2.0;4.0])
