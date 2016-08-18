#include <boost/variant.hpp>

#include <functional>
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <tuple>
#include <exception>
#include <cassert>

//A position describes a point in a source file
struct position {
  std::string pos_fname; //the file
  int pos_lnum; //the line number
  int pos_bol;  //offset of the line from the start of the lexbuf
  int pos_cnum; //offset of the position from the beginning of the lexbuf
  // the difference `pos_cnum` - `pos_bol` is the character offset
  // within the line
  position (std::string const& pos_fname
           , int pos_lnum, int pos_bol, int pos_cnum) 
    : pos_fname{pos_fname}
    , pos_lnum{pos_lnum}, pos_bol{pos_bol}, pos_cnum{pos_cnum}
  {}
};

//A type representing the span of two positions
struct location {
  position loc_start; //start position
  position loc_end;  //end position
  bool loc_ghost; //if `true` then a "ghost" range
  //(Ghost expressions and patterns do not appear explictily in source
  //files
  location (position const& loc_start
          , position const& loc_end, bool loc_ghost=false) 
    : loc_start{loc_start}, loc_end{loc_end}, loc_ghost{loc_ghost}
  {}
    
};

//Support for located errors
struct error_t {
  location loc;
  std::string msg;
  error_t (location const& loc, std::string const& msg)
    : loc{loc}, msg{msg}
  {}
};

//A prefix for error messages
char const* error_prefix="Error";

//Perform some formatting operations on a buffer then pass control to
//a continuation
template <class B, class K, class F>
auto ksprintf (B before, K k, F fmt) {
  //Create an in-memory buffer for formatting
  std::ostringstream os;
  //If `before` is non-empty, call it on the buffer
  if (!!before) {
    (*before) (os);
  }
  //Do the formatting operations
  fmt (os);
  //Finally, invoke the continuation on the result
  return k (os.str());
}

//Produce an `error_t` by way of formatting operations to produce the
//`msg` field of the result. The formatting operations include
//prefixing the `msg` field with the `error_prefix` string
template <class F>
error_t errorf_prefixed (location const& loc, F fmt) {
  return ksprintf (
     &[](std::ostream& os) { os << error_prefix << ": "; }
   , [&loc](std::string const& msg) { return error_t{loc, msg}; }
   , fmt);
}

//Make an `error_t` of an exception of type `E` by way of invoking the
//`printer` on the `E` to compute the `msg` field of the resulting
//`error`
template <class P, class E>
error_t error_of_printer (
      location const& loc
    , P printer
    , E const& e) {
   return errorf_prefixed (loc
     , [printer,&e](std::ostream& os) { return printer (os, e); }
    );
}

//Message constants
char const* const msg_file = "File \"";
char const* const msg_line = "\", line ";
char const* const msg_chars= ", characters ";
char const* const msg_to = "-";
char const* const msg_colon = ":";

//Compute file, line, char from the given position
inline 
std::tuple <std::string, int, int> 
get_pos_info (position const& pos) {
  return std::make_tuple (
      pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol);
}

//Print a location on a stream in terms of file, line and character
//numbers
std::ostream& operator << (std::ostream& os, location const& loc) {
  std::string file;
  int line;
  int start_char;
  std::tie (file, line, start_char) = get_pos_info (loc.loc_start);
  int end_char = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + start_char;
  os << msg_file << file << msg_line << line;
  if (start_char >= 0) {
    os << msg_chars << start_char << msg_to << end_char;
  }

  return os;
}

//A mutable list of exception handlers
using exception_handler = std::function<error_t()>;
using exception_handlers_t = std::vector<exception_handler>;
exception_handlers_t exception_handlers;

//A function to add a handler to the list
inline std::size_t register_error_of_exn (exception_handler const& h) {
  exception_handlers.push_back (h);
  return exception_handlers.size ();
}

//Walk the list looking for a handler. Assumes the function is only
//called in a context where there is a "currently handled" exception
error_t error_of_exn () {
  exception_handlers_t::const_iterator i = 
    exception_handlers.begin (), end = exception_handlers.end ();
  while (i != end) {
    try {
      return (*i) ();
    }
    catch (...) {
      ++i;
    }
  }
  throw; //No handler found
}

//A default error repoting function
std::ostream& operator << (std::ostream& os, error_t const& e) {
  return os << e.loc << msg_colon << '\n' << e.msg << std::endl;
}

//Hook to intercept error reporting
using error_reporter_t = std::ostream& (*)(std::ostream&, error_t const&);
error_reporter_t error_reporter[1]= { 
  static_cast<error_reporter_t>(&operator <<)//default error reporter
};

//Use the currently installed error reporter to print an error on the
//provided stream
std::ostream& report_error (std::ostream& os, error_t const& err) {
  return (*error_reporter) (os, err);
}

//Try to write an error report for a "currently handled
//exception". The exception will be re-thrown if no handler can be
//found
std::ostream& report_exception (std::ostream& os) {
  return report_error (os, error_of_exn ());
}

//-- 

//Test

//The cases of lexer errors

struct illegal_character {
  char c;
  illegal_character(char c) : c{c}
  {}
};

struct unterminated_comment {};

using lexer_error = boost::variant<illegal_character, unterminated_comment>;

//Write a message to the stream explaining the meaning of a lexer
//error
void report_lexer_error (std::ostream& os, lexer_error const& l) {
  if (illegal_character const* p = 
      boost::get<illegal_character>(&l)){
    os << "Illegal character (" << p->c << ")";
    return;
  }

  if (unterminated_comment const* p = 
      boost::get<unterminated_comment>(&l)){
    p;
    os << "Comment not terminated";
    return;
  }

  assert (false); //match non-exhaustive
}

//The lexer exception type
struct lexer_exception {
  location loc;
  lexer_error err;

  lexer_exception (location const& loc, lexer_error const& err) 
    : loc{loc}, err{err}
  {}
};

//A handler of lexer exceptions
error_t lexer_exceptions () {
  try {
    throw;
  }
  catch (lexer_exception const& l) {
    return error_of_printer (l.loc, report_lexer_error, l.err);
  }
}

//Register a handler for lexer exceptions
int leht = register_error_of_exn (exception_handler{lexer_exceptions});

//The cases of syntax errors

struct other {
  location loc;

  other (location const& loc) : loc{loc}
  {}
};

struct not_expecting {
  location loc;
  std::string nonterm;
  
  not_expecting (location const& loc, std::string const& nonterm)
    : loc{loc}, nonterm{nonterm}
  {}
};

using syntax_error = boost::variant<other, not_expecting>;

//Produce a located error from a syntax error
error_t prepare_syntax_error (syntax_error const& error) {
  if (other const* const p = boost::get<other>(&error)){
    return errorf_prefixed (
      p->loc, 
      [=](std::ostream& os) {  os << "Syntax error"; });
  }

  if (not_expecting const* const p = boost::get<not_expecting>(&error)){
    return errorf_prefixed (
       p->loc, 
       [=](std::ostream& os) {  
         os << "Syntax error: \"" << p->nonterm << "\" not expected."; });
  }

  assert (false); //match non-exhaustive
}

//Syntax exception type
struct syntax_exception {
  syntax_error error;

  syntax_exception (syntax_error const& error) : error{error}
  {}
};

//A handler of syntax exceptions
error_t syntax_exceptions () {
  try{
    throw;
  }
  catch (syntax_exception const& s) {
    return prepare_syntax_error (s.error);
  }
}
//Register a handler for syntax exceptions
int seht = register_error_of_exn (exception_handler{syntax_exceptions});

int main () {

  position start {"<string>", 1, 0, 0}; 
  position end{start};
  location loc{start, end};

  try{

    //report lexer exception
    try {
      throw lexer_exception (loc, lexer_error{illegal_character{'-'}});
    }
    catch (...) {
      report_exception (std::cout);
    }

    //report syntax exception
    try {
      throw syntax_exception (syntax_error{not_expecting{loc, "foo"}});
    }
    catch (...) {
      report_exception (std::cout);
    }

    //this should escape
    try {
      throw std::runtime_error{"Intentional"};
    }
    catch (...) {
      report_exception (std::cout);
    }

  }
  catch (std::runtime_error const& e) {
    std::cerr << "Runtime error: " << e.what () << '\n';
  }
  catch (...) {
    std::cerr << "Unhandled excpeption\n";
  }

  return 0;
}
