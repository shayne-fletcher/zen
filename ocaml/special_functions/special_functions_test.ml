let black_scholes ~spot ~strike ~r ~vol ~t ~cp =
  let open Special_functions in
  let d1 = (log (spot/.strike) +. 
              (r +. 0.5*.(vol*.vol))*.t)/.(vol*.(sqrt t)) in
  let d2 = d1 -. vol*.(sqrt t) 
  in
    cp*.spot*.(norm_cdf (cp*.d1)) -. 
       cp*.strike*.(exp ((~-.r)*.t))*.(norm_cdf (cp*.d2))

let _ = 
  Printf.printf "%.8f\n" 
    (black_scholes ~spot:42. ~strike:40. ~r:0.1 ~vol:0.2 ~t:0.5 ~cp:1.0) ;
  Printf.printf "%.8f\n" 
    (black_scholes ~spot:42. ~strike:40. ~r:0.1 ~vol:0.2 ~t:0.5 ~cp:(~-.1.0))
