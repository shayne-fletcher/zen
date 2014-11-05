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

let geometric_mean t =
  (*http://en.wikipedia.org/wiki/Geometric_mean

    The geometric mean is defined as the nth root (where n is the
    count of numbers) of the product of the numbers.

  *)
  let n = List.length t in
  let prod = List.fold_left (fun acc xi -> acc *. xi) 1.0 t in
  prod ** (1.0/.(float_of_int n))

let quadratic_mean t =
  (*http://en.wikipedia.org/wiki/Standard_deviation

  In mathematics, the root mean square (abbreviated RMS or rms),
  also known as the quadratic mean, is a statistical measure of the
  magnitude of a varying quantity. It is especially useful when
  variates are positive and negative, e.g., sinusoids. RMS is used
  in various fields, including electrical engineering.  It can be
  calculated for a series of discrete values or for a continuously
  varying function. Its name comes from its definition as the square
  root of the mean of the squares of the values.

  *)
  let squares = List.fold_left 
    (fun acc xi -> acc @ [xi *. xi]) [] t in
  sqrt (arithmetic_mean squares)

let power_mean p t =
  (*http://en.wikipedia.org/wiki/Generalized_mean

    In mathematics, generalized means are a family of functions for
    aggregating sets of numbers, that include as special cases the
    arithmetic, geometric, and harmonic means

  *)
  let powers = List.fold_left 
    (fun acc xi -> acc @ [( ** ) xi p]) [] t in
    (arithmetic_mean powers)**(1.0/.p)

let standard_deviation t =
  (*http://en.wikipedia.org/wiki/Standard_deviation

    For a finite set of numbers, the standard deviation is found by
    taking the square root of the average of the squared differences
    of the values from their average value.

   *)
 let av = arithmetic_mean t in
 let squared_diffs = List.fold_left (fun acc xi -> ((xi -. av) *. (xi -. av)) :: acc) [] t
 in sqrt (arithmetic_mean squared_diffs)

let unbiased_standard_deviation t =
  (*http://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation

    In statistics and in particular statistical theory, unbiased
    estimation of a standard deviation is the calculation from a
    statistical sample of an estimated value of the standard deviation
    (a measure of statistical dispersion) of a population of values,
    in such a way that the expected value of the calculation equals
    the true value.

  *)
 let av = arithmetic_mean t in
 let squared_diffs = List.fold_left (fun acc xi -> ((xi -. av) *. (xi -. av)) :: acc) [] t
 in sqrt ((sum squared_diffs)/.((float_of_int (List.length t)) -. 1.0))

let correlation_coefficient x y =
  (*http://en.wikipedia.org/wiki/Correlation_and_dependence

    The most familiar measure of dependence between two quantities is
    the Pearson product-moment correlation coefficient, or "Pearson's
    correlation coefficient", commonly called simply "the correlation
    coefficient". It is obtained by dividing the covariance of the two
    variables by the product of their standard deviations.  
  *) 

  let x_b = arithmetic_mean x in
  let y_b = arithmetic_mean y in
  let s_x = unbiased_standard_deviation x in
  let s_y = unbiased_standard_deviation y in

  if s_x = 0. || s_y = 0. then 0.
  else
    let f acc x_i y_i =
      acc +. ((x_i -. x_b) *. (y_i -. y_b)) in
    let n = float_of_int (List.length x) in
    let s = List.fold_left2 f 0.0 x y  in
    s/.((n -. 1.) *. s_x *. s_y)

