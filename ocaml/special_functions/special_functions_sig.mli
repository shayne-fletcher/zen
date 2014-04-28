(**[Special_functions] module interface*)

(**Provides a small number of high quality special functions,
   initially these were concentrated on functions used in statistical
   applications along with those in the Technical Report on C++
   Library Extensions. See
   {{:http://www.boost.org/doc/libs/1_55_0/libs/math/doc/html/special.html}
   Boost Math Toolkit}*)
module type S = sig

    val norm_cdf : float -> float
    (**The cumulative distribution function of the standard normal
      distribution*)

end
