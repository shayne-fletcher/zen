(*
#load "unix.cma";;
#load "str.cma";;
#load "calendarLib.cma";;
*)

(*In this version, only weekends are holidays.*)
let is_business_day : CalendarLib.Date.t -> string -> bool = 
  fun t (loc:string) ->
    let f = function 
      | CalendarLib.Date.Sat -> false 
      | CalendarLib.Date.Sun -> false 
      | _ -> true
    in f (CalendarLib.Date.day_of_week t)
;;

type shift_convention =
| NoShift
| Following
| ModifiedFollowing
| Preceding
| ModifiedPreceding
;;

let string_of_shift_convention : shift_convention -> string = 
  function 
    | NoShift -> "NO_SHIFT"
    | Following -> "FOLLOWING"
    | ModifiedFollowing -> "MODIFIED_FOLLOWING"
    | Preceding -> "PRECEDING"
    | ModifiedPreceding -> "MODIFIED_PRECEDING"
;;

let shift_convention_of_string : string -> shift_convention =
  function
    | "NO_SHIFT" -> NoShift
    | "FOLLOWING" -> Following
    | "MODIFIED_FOLLOWING" -> ModifiedFollowing
    | "PRECEDING" -> Preceding
    | "MODIFIED_PRECEDING" -> ModifiedPreceding
    | s -> failwith ("Couldn't convert \""^s^"\" to a shift convention")
;;

let rec shift_following : CalendarLib.Date.t -> string -> CalendarLib.Date.t = 
  fun t loc ->
    if is_business_day t loc then
      t
    else
     shift_following (CalendarLib.Date.add t (CalendarLib.Date.Period.day 1)) loc
  ;;

let rec shift_preceding : CalendarLib.Date.t -> string -> CalendarLib.Date.t = 
  fun t loc ->
    if is_business_day t loc then
      t
    else
      shift_preceding (CalendarLib.Date.add t (CalendarLib.Date.Period.day (-1))) loc
  ;;

let shift_modified_following : CalendarLib.Date.t -> string -> CalendarLib.Date.t =
  fun t loc ->
    let s = shift_following t loc
    in
      let m  = CalendarLib.Date.month t
      and n  = CalendarLib.Date.month s
      in 
        if m = n 
	then 
	  s
	else 
	  shift_preceding t loc
  ;;
  
let shift_modified_preceding : CalendarLib.Date.t -> string -> CalendarLib.Date.t =
  fun t loc ->
    let s = shift_preceding t loc
    in
      let m  = CalendarLib.Date.month t
      and n  = CalendarLib.Date.month s
      in 
        if m = n 
	then 
	  s
	else 
	  shift_following t loc
  ;;
  
let shift : CalendarLib.Date.t->shift_convention->string->CalendarLib.Date.t = 
  fun t s loc ->
       match s with
       | NoShift -> t
       | Following -> shift_following t loc
       | Preceding -> shift_preceding t loc
       | ModifiedFollowing -> shift_modified_following t loc
       | ModifiedPreceding -> shift_preceding t loc
  ;;
     
type day_count = 
| DC_30_360
| DC_ACT_360
| DC_ACT_365
| DC_ACT_ACT
;;

let string_of_day_count : day_count -> string =
  function 
  | DC_30_360 -> "DC_30_360"
  | DC_ACT_360 -> "DC_ACT_360"
  | DC_ACT_365 -> "DC_ACT_365"
  | DC_ACT_ACT -> "DC_ACT_ACT"
;;

let day_count_of_string : string -> day_count =
  function 
  |  "DC_30_360" -> DC_30_360
  |  "DC_ACT_360" -> DC_ACT_360
  |  "DC_ACT_365" -> DC_ACT_365
  | "DC_ACT_ACT" -> DC_ACT_ACT
  | s -> failwith ("Couldn't convert \""^s^"\" to a day count convention")
;;

let year_fraction_act_360 : (CalendarLib.Date.t * CalendarLib.Date.t) -> float =
  fun (s, u) ->
    (float_of_int (CalendarLib.Date.Period.safe_nb_days (CalendarLib.Date.sub u s)))/. 360.0
;;

let year_fraction_act_365 : (CalendarLib.Date.t * CalendarLib.Date.t) -> float =   
  fun (s, u) ->
    (float_of_int (CalendarLib.Date.Period.safe_nb_days (CalendarLib.Date.sub u s)))/. 365.0
;;

let year_fraction_30_360 : (CalendarLib.Date.t * CalendarLib.Date.t) -> float =
  fun (s, u) ->
    let sy = CalendarLib.Date.year s
    and sm = CalendarLib.Date.int_of_month (CalendarLib.Date.month s)
    and sd = CalendarLib.Date.day_of_month s
    and uy = CalendarLib.Date.year u
    and um = CalendarLib.Date.int_of_month (CalendarLib.Date.month u)
    and ud = CalendarLib.Date.day_of_month u
    in
      let d1 = if sd != 31 then sd else 30
      and d2 = if ud != 31 then ud else 30
      in
        let a : float = float_of_int (d2 - d1)
        and b : float = (float_of_int (um - sm))*.30.0
        and c : float = (float_of_int (uy - sy))*.360.0
        in (a +. b +. c) /. 360.0
;;

let year_fraction_act_act : (CalendarLib.Date.t*CalendarLib.Date.t) -> float =
  fun (s, u) ->
    let sy = CalendarLib.Date.year s
    and uy = CalendarLib.Date.year u
    in
    if sy != uy then
      let uy_s = CalendarLib.Date.make uy 1 1
      and sy_end = CalendarLib.Date.make sy 12 31
      and sy_days = if CalendarLib.Date.is_leap_year sy then 366 else 365
      and uy_days = if CalendarLib.Date.is_leap_year uy then 366 else 365
      in
        let n1 = CalendarLib.Date.Period.safe_nb_days (CalendarLib.Date.sub sy_end s)
	and n2 = CalendarLib.Date.Period.safe_nb_days (CalendarLib.Date.sub u uy_s)
	in
	  float_of_int (n1) /. (float_of_int sy_days) +. float_of_int (uy - sy - 1) +. float_of_int (n2)/.(float_of_int uy_days)
    else
      let days = if CalendarLib.Date.is_leap_year sy then 366 else 365
      in float_of_int (CalendarLib.Date.Period.safe_nb_days (CalendarLib.Date.sub u s)) /. (float_of_int days)
;;

let year_fraction : (CalendarLib.Date.t * CalendarLib.Date.t) -> day_count -> float = 
  fun dt code ->
    match code with
    | DC_30_360 -> year_fraction_30_360 dt
    | DC_ACT_360 -> year_fraction_act_360 dt
    | DC_ACT_365 -> year_fraction_act_365 dt
    | DC_ACT_ACT -> year_fraction_act_act dt
;;

let day_diff : CalendarLib.Date.t -> CalendarLib.Date.t -> int =
  fun to_ from ->
    CalendarLib.Date.Period.safe_nb_days (CalendarLib.Date.sub to_ from)
;;

let year_diff : CalendarLib.Date.t -> CalendarLib.Date.t -> float =
  fun to_ from ->
    (float_of_int (day_diff to_ from))/.365.0
;;
