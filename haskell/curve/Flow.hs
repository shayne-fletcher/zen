module Flow(
   Flow(..)
 , AccrualStart
 , AccrualEnd
 , Accrual
 , Pay
 , generate_flows) where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Date

data Flow = Flow {
    start    :: AccrualStart
  , end      :: AccrualEnd
  , accrual  :: Float
  , pay      :: Day
  } deriving (Eq, Ord, Show, Read)

type AccrualStart = Day
type AccrualEnd   = Day
type Accrual      = Float
type Pay          = Day

generate_flows_
  :: (Num a) =>
     t
     -> Day
     -> Day
     -> a
     -> a
     -> (a -> t -> Day)
     -> DayCount
     -> ShiftConvention
     -> t1
     -> ShiftConvention
     -> t2
     -> [Flow]
generate_flows_
  start
  end
  day
  i
  period 
  add_duration
  acc_basis 
  acc_shift_conv 
  acc_hols 
  pay_shift_conv
  pay_hols =
    if day >= end
    then 
      []
    else
      let roll_start = add_duration (i*period) start
          roll_end   = add_duration ((i + 1)*period) start
          acc_start  = shift roll_start acc_shift_conv acc_hols
          acc_end    = shift roll_end acc_shift_conv acc_hols
          alpha      = year_fraction acc_start acc_end acc_basis
          pay        = shift roll_end pay_shift_conv pay_hols
       in [Flow acc_start acc_end alpha pay] ++ 
          generate_flows_ 
                     start
                     end
                     roll_end
                     (i + 1) 
                     period 
                     add_duration
                     acc_basis 
                     acc_shift_conv
                     acc_hols 
                     pay_shift_conv
                     pay_hols

generate_flows
  :: Day
     -> Day
     -> Integer
     -> Resolution
     -> DayCount
     -> ShiftConvention
     -> t
     -> ShiftConvention
     -> t1
     -> [Flow]
generate_flows
  start
  end
  period 
  resolution
  acc_basis 
  acc_shift_conv 
  acc_hols 
  pay_shift_conv
  pay_hols =
    generate_flows_ 
              start end 
              start   0 
              period 
              (add_duration_func resolution) 
              acc_basis 
              acc_shift_conv 
              acc_hols 
              pay_shift_conv 
              pay_hols
        
-- flows = generate_flows (fromGregorian 2007 6 29) (fromGregorian 2017 6 29) 6 MONTHS DC_30360 MODIFIED_FOLLOWING Nothing MODIFIED_FOLLOWING Nothing
