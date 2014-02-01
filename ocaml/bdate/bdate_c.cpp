#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
/*
Reflect Boost.Date_time boost::gregorian::date.
*/
#include <boost/date_time/gregorian/gregorian.hpp>
/*
Use Unix.tm for intermediate representation

type tm = {
  	tm_sec : int;	(*	Seconds 0..60	*)
  	tm_min : int;	(*	Minutes 0..59	*)
  	tm_hour : int;	(*	Hours 0..23	*)
  	tm_mday : int;	(*	Day of month 1..31	*)
  	tm_mon : int;	(*	Month of year 0..11	*)
  	tm_year : int;	(*	Year - 1900	*)
  	tm_wday : int;	(*	Day of week (Sunday is 0)	*)
  	tm_yday : int;	(*	Day of year 0..365	*)
  	tm_isdst : bool;	(*	Daylight time savings in effect	*)
}
*/
extern "C" value caml_boost_gregorian_day_clock_local_day ()
{
  value res=0;
  struct caml__roots_block blk, *caml__frame=caml_local_roots;
  blk.next = caml_local_roots;
  blk.nitems = 1;
  blk.ntables = 1;
  blk.tables [0] = &res;
  caml_local_roots = &blk;

  struct tm t = 
    boost::gregorian::to_tm (
      boost::gregorian::day_clock::local_day ());

  res = caml_alloc (9, 0);
  Store_field (res, 0, Val_int (t.tm_sec));
  Store_field (res, 1, Val_int (t.tm_min));
  Store_field (res, 2, Val_int (t.tm_hour));
  Store_field (res, 3, Val_int (t.tm_mday));
  Store_field (res, 4, Val_int (t.tm_mon));
  Store_field (res, 5, Val_int (t.tm_year));
  Store_field (res, 6, Val_int (t.tm_wday));
  Store_field (res, 7, Val_int (t.tm_yday));
  Store_field (res, 8, Val_bool (false)); //t.tm_isddst is always -1

  value tmp = res;
  caml_local_roots = caml__frame;

  return tmp;

}
