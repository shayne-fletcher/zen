#include <boost/range.hpp>
#include <boost/assign/list_of.hpp>

#include <string>
#include <numeric>
#include <cstring>
#include <iostream>
#include <list>

/*
let concat sep l =
  match l with
    [] -> ""
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = create (!len + length sep * (!num - 1)) in
      unsafe_blit hd 0 r 0 (length hd);
      let pos = ref(length hd) in
      List.iter
        (fun s ->
          unsafe_blit sep 0 r !pos (length sep);
          pos := !pos + length sep;
          unsafe_blit s 0 r !pos (length s);
          pos := !pos + length s)
        tl;
      r
 */

namespace string_util /*In honor of Stefano :)*/ 
{
  template <class RgT>
  std::string concat (std::string const& sep, RgT lst)
  {
    using boost::empty;
    using boost::begin;
    using boost::end;

    if (boost::empty (lst))
      return std::string ("");

    std::size_t num = 0, len = 0;
    std::accumulate (
      boost::begin (lst), boost::end (lst), 0,
      [&](int _, std::string const& s) -> 
      int { ++num, len += s.size(); return _; } );
    std::string r(len + sep.size () * (num - 1), '\0');
    std::string const& hd = *(boost::begin (lst));
    std::memcpy ((void*)(r.data ()), (void*)(hd.data ()), hd.size());
    std::size_t pos = hd.size();
    std::accumulate (
      boost::next (boost::begin (lst)), boost::end (lst), 0,
      [&](int _, std::string const& s) -> 
      int {
        std::memcpy((void*)(r.data ()+pos),(void*)(sep.data()),sep.size ());
        pos += sep.size ();
        std::memcpy ((void*)(r.data()+pos),(void*)(s.data()),s.size ());
        pos += s.size ();
        return _; });
  
  return r;
}
}//namespace<string_util>

int main ()
{
  std::list<std::string> lst = boost::assign::list_of ("foo")("bar")("baz");
  std::cout << string_util::concat (",", lst);

  return 0;
}
