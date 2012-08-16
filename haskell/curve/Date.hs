module Date (
    Resolution(..)
  , ShiftConvention(..)
  , DayCount(..)
  , is_business_day
  , shift
  , year_fraction
  , add_duration_func) where

import Data.Time
import Data.Time.Calendar.OrdinalDate

-- | Date resolutions

data Resolution = DAYS | MONTHS | YEARS deriving (Eq, Ord, Show, Read)

-- | Market shift conventions (date roll conventions).

data ShiftConvention = 
   NONE               | 
   FOLLOWING          | 
   MODIFIED_FOLLOWING | 
   PRECEDING          | 
   MODIFIED_PRECEDING
   deriving (Eq, Ord, Show, Read)

-- | Test whether the given day is a business day. In this version,
-- | only weekends are considered holidays.

is_business_day :: Day -> t -> Bool
is_business_day t _ = (d /=6) && (d /= 7) where d = (snd.mondayStartWeek) t

-- | Shift a date according to the FOLLOWING convention.

shift_following :: Day -> t -> Day
shift_following t hols = 
  if (is_business_day t hols) then 
    t 
  else 
    shift_following (addDays 1 t) hols

-- | Shift a date according to the PRECEDING convention.

shift_preceding :: Day -> t -> Day
shift_preceding t  hols= 
  if (is_business_day t hols) then 
    t 
  else 
    shift_preceding (addDays (-1) t) hols

-- | Shift a date according to the MODIFIED_FOLLOWING convention.

shift_modified_following :: Day -> t -> Day
shift_modified_following t hols
  | m == n    = s
  | otherwise = shift_preceding t hols
  where s = shift_following t hols
        (_, m, _) = toGregorian s
        (_, n, _) = toGregorian t

-- | Shift a date according to the MODIFIED_PRECEDING convention.

shift_modified_preceding :: Day -> t -> Day
shift_modified_preceding t hols
  | m == n    = t
  | otherwise = shift_following t hols
  where s = shift_preceding t hols
        (_, m, _) = toGregorian s
        (_, n, _) = toGregorian t

-- | Shift a date according to a shift convention.

shift :: Day -> ShiftConvention -> t -> Day
shift t method hols = 
  case method of
      NONE               -> t
      FOLLOWING          -> shift_following t hols
      PRECEDING          -> shift_preceding t hols
      MODIFIED_FOLLOWING -> shift_modified_following t hols
      MODIFIED_PRECEDING -> shift_modified_preceding t hols

-- | Market daycount conventions (accrual basis).

data DayCount = 
  DC_30360  | 
  DC_ACT360 | 
  DC_ACT365 | 
  DC_ACTACT
  deriving (Eq, Ord, Show, Read)

-- | Year fraction under the ACT/360 convention.

year_fraction_act360 :: (Fractional b) => Day -> Day -> b
year_fraction_act360 s u = fromIntegral (diffDays u s)/360.0

-- | Year fraction under the ACT/365 convention.

year_fraction_act365 :: (Fractional b) => Day -> Day -> b
year_fraction_act365 s u = fromIntegral (diffDays u s)/365.0

-- | Year fraction under the ACT/ACT convention.

year_fraction_actact :: (Fractional b) => Day -> Day -> b
year_fraction_actact s u
  | sy /= uy =
      let uy_s     = fromGregorian uy  1  1
          sy_end   = fromGregorian sy 12 31
          sy_days  = if isLeapYear sy then 366 else 365
          uy_days  = if isLeapYear uy then 366 else 365
      in fromIntegral (diffDays sy_end s)/sy_days + 
         fromIntegral (uy - sy - 1)               + 
         fromIntegral (diffDays u uy_s)/uy_days

  | otherwise = 
      let days = if isLeapYear sy then 366.0 else 365.0
      in fromIntegral (diffDays u s) / days

  where (sy, sm, sd) = toGregorian s
        (uy, um, ud) = toGregorian u

-- | Year fraction under the 30/360 convention.

year_fraction_30360 :: (Fractional b) => Day -> Day -> b
year_fraction_30360 s u =
   let (sy, sm, sd) = toGregorian s
       (uy, um, ud) = toGregorian u
       d1 = if sd /= 31 then sd else 30
       d2 = if ud /= 31 then ud else 30 
   in (fromIntegral (d2 - d1) + 
       30.0*fromIntegral (um - sm) + 
       360.0*fromIntegral (uy - sy))/360.0

-- | Year fraction under a daycount convention.

year_fraction :: (Fractional b) => Day -> Day -> DayCount -> b
year_fraction s u basis = 
  case basis of
    DC_30360   -> year_fraction_30360 s u
    DC_ACT360  -> year_fraction_act360 s u
    DC_ACT365  -> year_fraction_act365 s u
    DC_ACTACT  -> year_fraction_actact s u

add_duration_func :: Resolution -> Integer -> Day -> Day
add_duration_func resolution =
  case resolution of 
    DAYS   -> addDays
    MONTHS -> addGregorianMonthsClip
    YEARS  -> addGregorianYearsClip


-- *Main> let begin = fromGregorian 2004 11 21
-- *Main> let until = addGregorianMonthsClip 6 begin
-- *Main> year_fraction begin until DC_30360
-- 0.5
-- *Main> year_fraction begin until DC_ACT365
-- 0.4958904109589041
-- *Main> year_fraction begin until DC_ACTACT
-- 0.4928512613219552
