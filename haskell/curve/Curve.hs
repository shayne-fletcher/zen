-- | Curve.hs
-- |
-- | Build a discount factor curve from cash deposits and interest rate swaps.

module Curve where

import Flow
import Date
import Interpolation
import Roots
import List
import Data.Maybe
import Data.Time
import Data.Time.Calendar.OrdinalDate

days_diff = diffDays
year_diff to from = (fromIntegral (days_diff to from))/365.0

type Coupon = Float
type Flows  = [Flow]

-- | Cash deposits

data CashDeposit = 
  CashDeposit 
  { 
    coupon :: Coupon
  , flow   :: Flow 
  }  deriving (Show, Read)

cash_deposit spec =
  CashDeposit 
    coupon 
    (head 
      (generate_flows 
                  start
                  end
                  period
                  resolution
                  acc_basis
                  acc_shift_conv
                  acc_hols
                  pay_shift_conv
                  pay_hols))
  where
     (   coupon
       , start
       , end
       , period
       , resolution
       , acc_basis
       , acc_shift_conv
       , acc_hols
       , pay_shift_conv
       , pay_hols) = spec

-- | Interest rate swaps

data FloatLeg = FloatLeg Flows deriving (Show, Read)

float_leg spec =
   FloatLeg
      (generate_flows 
                  start 
                  end  
                  period 
                  resolution 
                  acc_basis 
                  acc_shift_conv 
                  acc_hols 
                  pay_shift_conv 
                  pay_hols)
   where
     (   start
       , end
       , period
       , resolution
       , acc_basis
       , acc_shift_conv
       , acc_hols
       , pay_shift_conv
       , pay_hols) = spec

data FixedLeg =  FixedLeg Coupon Flows deriving (Show, Read)

fixed_leg spec =
   FixedLeg
      coupon 
      (generate_flows 
                  start 
                  end  
                  period 
                  resolution 
                  acc_basis 
                  acc_shift_conv 
                  acc_hols 
                  pay_shift_conv 
                  pay_hols)
   where
     (   coupon
       , start
       , end
       , period
       , resolution
       , acc_basis
       , acc_shift_conv
       , acc_hols
       , pay_shift_conv
       , pay_hols) = spec

data Swap = Swap FixedLeg FloatLeg deriving (Show, Read)

interest_rate_swap spec = 
  Swap 
    ( fixed_leg 
          ( coupon
          , start
          , end
          , period
          , res
          , accrual_basis
          , accrual_shift_convention
          , accrual_hols
          , pay_shift_convention
          , pay_hols
          )
    )
    ( float_leg      
          ( start
          , end
          , period
          , res
          , accrual_basis
          , accrual_shift_convention
          , accrual_hols
          , pay_shift_convention
          , pay_hols
          )
     ) 
  where 
    (   coupon
      , start
      , end
      , period
      , res
      , accrual_basis
      , accrual_shift_convention
      , accrual_hols
      , pay_shift_convention
      , pay_hols) = spec

swap_maturity today swap = 
  (pay, e)
  where 
  Swap fixed _     =  swap
  FixedLeg _ flows = fixed
  Flow _ _ _ pay   = last flows
  e = year_diff pay today

-- | Discount factor curve

data Curve   = Curve Dates Offsets Factors Interpolation
type Dates   = [Day]
type Offsets = [Float]
type Factors = [Float]
type Interpolation = (Float -> [Float] -> [Float] -> Float)

show_curve (Curve dates _ factors f) = show (zip dates factors)
instance Show Curve where show = show_curve

write_table [] = ""
write_table (x:xs) = 
  show (fst x) ++ "    " ++ show (snd x) ++ "\n" ++ (write_table xs)
write_curve (Curve dates _ factors _) = 
  "t             P(t)\n" ++ 
  "------------------\n" ++
  write_table (zip dates factors)
print_curve = putStr.write_curve

df (Curve _ xs ys f) x
   | isJust index  = ys!!(fromJust index)
   | otherwise     = f x xs ys
   where index = findIndex (== x) xs

curve_date (Curve dates _ _ _) = head dates

curve_append (Curve ds xs ys f) 
  mat x y = Curve (ds++[mat]) (xs++[x]) (ys++[y]) f

-- | Bootstrap

class Appender a where bootstrap  :: Curve -> [a] -> Curve

-- | Bootstrap deposits

append_deposit crv deposit = 
  curve_append crv mat off fact
  where
     Curve dates xs ys f       = crv
     CashDeposit r flow        = deposit
     Flow start mat accrual _  = flow
     t                         = curve_date crv
     s                         = year_diff start t
     off                       = year_diff mat t
     ps                        = df crv s
     fact                      = ps/(1.0 + r*accrual)

instance Appender CashDeposit where bootstrap = foldl append_deposit 

-- | Boostrap interest rate swaps

value_swap crv swap = 
  (sum 
     (map 
        ((\crv flow ->
          let 
            Flow s e acc pay = flow
            ps               = df crv (year_diff s t)
            pe               = df crv (year_diff e t)
            libor            = (ps/pe - 1.0)/acc
            disc             = df crv (year_diff pay t)
          in disc*libor*acc) crv)
        flt_flows
     )
  ) -
  (sum
     (map 
       ((\cpn crv flow ->
         let
           Flow _ _ acc pay   = flow
           disc               = df crv (year_diff pay t)
         in disc*cpn*acc) cpn crv)
      fxd_flows
     )
  ) 
  where t = curve_date crv
        Swap       fixed float =  swap
        FixedLeg cpn fxd_flows = fixed
        FloatLeg     flt_flows = float

append_swap crv swap =
  curve_append crv mat off ((a + b)/2.0)
  where 
     Curve dates xs ys f  = crv
     today = curve_date crv
     (a, b, _) =  
       bisect 
         (\y -> value_swap (curve_append crv mat off y) swap)
         0 1 tol max_its
     tol = 1.0e-6
     max_its = 5000
     (mat, off) = swap_maturity today swap

instance Appender Swap where bootstrap = foldl append_swap

-- Test

add_days  = addDays
add_years = addGregorianYearsClip
add_mnths = addGregorianMonthsClip
today     = fromGregorian 2004 8 24

mk_deposit rate end period res = 
  cash_deposit 
     ( rate
     , today
     , end
     , period
     , res
     , DC_ACT360
     , MODIFIED_FOLLOWING
     , Nothing
     , MODIFIED_FOLLOWING
     , Nothing
     )

mk_swap cpn end period res = 
  interest_rate_swap
    ( cpn
    , today
    , end
    , period
    , res
    , DC_ACT360
    , MODIFIED_FOLLOWING
    , Nothing
    , MODIFIED_FOLLOWING
    , Nothing
    )

deposits = [ 
             mk_deposit 0.00040 (add_days  1 today) 1 DAYS    -- O/N
           , mk_deposit 0.00040 (add_mnths 1 today) 1 MONTHS  --  1m
           , mk_deposit 0.00054 (add_mnths 2 today) 2 MONTHS  --  2m
           , mk_deposit 0.00054 (add_mnths 3 today) 3 MONTHS  --  3m
           , mk_deposit 0.00066 (add_mnths 6 today) 6 MONTHS  --  6m
           ]

swaps    = [ 
             mk_swap 0.0007600 (add_years   1 today) 1 YEARS  --  1y
           , mk_swap 0.0010400 (add_mnths  18 today) 6 MONTHS -- 18m
           , mk_swap 0.0015100 (add_years   2 today) 6 MONTHS --  2y
           , mk_swap 0.0026400 (add_years   3 today) 6 MONTHS --  3y
           , mk_swap 0.0039800 (add_years   4 today) 6 MONTHS --  4y
           , mk_swap 0.0054900 (add_years   5 today) 6 MONTHS --  5y
           , mk_swap 0.0071300 (add_years   6 today) 6 MONTHS --  6y
           , mk_swap 0.0088000 (add_years   7 today) 6 MONTHS --  7y
           , mk_swap 0.0104400 (add_years   8 today) 6 MONTHS --  8y
           , mk_swap 0.0119400 (add_years   9 today) 6 MONTHS --  9y
           , mk_swap 0.0132500 (add_years  10 today) 6 MONTHS -- 10y
           , mk_swap 0.0154000 (add_years  12 today) 6 MONTHS -- 12y
           , mk_swap 0.0177600 (add_years  15 today) 6 MONTHS -- 15y
           , mk_swap 0.0204500 (add_years  20 today) 6 MONTHS -- 20y
           , mk_swap 0.0219900 (add_years  25 today) 6 MONTHS -- 25y
           ]

dfs_ crv start end day period add_duration
     | day < start || day >= end = []
     | otherwise = [(day, (df crv (year_diff day today)))] ++ 
                    (dfs_ crv start end 
                       (add_duration period day) period add_duration)
dfs crv start end period res = 
       dfs_ crv start end start period (add_duration_func res)

