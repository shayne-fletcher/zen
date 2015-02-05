//cl /Fesll.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 sll.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/optional.hpp>

#include <stdexcept>
#include <functional>
#include <sstream>

template <class T> using ptr_t = 
boost::variant <
    boost::shared_ptr<T>  
  , boost::weak_ptr<T>>;
template <class T> struct node;
template <class T> using list = ptr_t<node<T>>;

template <class T> list<T> nil (); 
template <class T> bool empty (list<T> l);
template <class T> list<T> cons (T val, list<T> l);
template <class T> T& hd (list<T> l);
template <class T> list<T>& tl (list<T> l);
template <class T> list<T> last (list<T> l);
template <class T> typename node<T>::weak_ptr ref (list<T> src);
template <class T> bool is_ref (list<T> src);
template <class T> list<T> copy (list<T> src);

// --

template <class T>
struct node {
  T data;
  ptr_t<node<T>> next;

  typedef boost::weak_ptr<node<T>> weak_ptr;
  typedef boost::shared_ptr<node<T>> shared_ptr;
};

namespace {
  //'get' at the raw pointer in the union of a smart/weak pointer
  template <class T>
  T* get (ptr_t<T> l) {
    if (boost::shared_ptr<T>* p=
        boost::get<boost::shared_ptr<T>>(&l)) {
      return p->get ();
    }
    return boost::get<boost::weak_ptr<T>>(l).lock ().get ();
  }
}//namespace<anonymous>

template <class T> list<T> nil (){ 
  return typename node<T>::shared_ptr (); 
}

template <class T> bool empty (list<T> l) { 
  return (get (l)) == nullptr; 
}

template <class T> list<T> cons (T val, list<T> l) {
  return node<T>::shared_ptr (new node<T>{val, l});
}

template <class T> T& hd (list<T> l) {   
  if (empty (l))
    throw std::runtime_error ("hd");
  return get (l) -> data;
}

template <class T> list<T>& tl (list<T> l) { 
  return get (l) -> next; 
}

template <class T> list<T> last (list<T> l) {
  if (empty (l))
    throw std::runtime_error ("last");
  if (boost::get<typename node<T>::weak_ptr>(&l))
    throw std::runtime_error ("last");

  list<T> t = tl (l);
  if (empty (t)) return l;

  return last (t);
}

template <class T> bool is_ref (list<T> src) {
  return boost::get<typename node<T>::weak_ptr>(&src)!=nullptr;
}

template <class T> typename node<T>::weak_ptr ref (list<T> src)  {
  return typename node<T>::weak_ptr(
           boost::get<typename node<T>::shared_ptr>(src));
}

struct not_found {};

//May not terminate if the list contains a cycle
template <class A, class B>
B assoc (A a, list<std::pair<A, B>> l) {
  if (empty (l))
    throw not_found ();
  std::pair<A, B> p=hd (l);
  if (p.first == a)
    return p.second;
  return assoc (a, tl (l));
}

namespace {

  template <class T> using maybe_shared_ptr = 
    boost::optional<boost::shared_ptr<T>>;

  template <class B> boost::optional<B> const none () { 
    return boost::optional<B>(); 
  }

  template <class B> boost::optional<B> some (B tok) { 
    return boost::optional<B>(tok); 
  }

  //Copy a list. If there is a cycle, nil terminate the result
  template <class T>
  list<T> copy_aux (list<T> src) {
    if (empty (src))
      return nil<T> (); //run out of list

    if (boost::get<node<T>::weak_ptr>(&src))
      return nil<T> (); //encountered a cycle

    return typename node<T>::shared_ptr (new node<T>{ hd (src), copy_aux (tl (src))});
  }

  //The idea is, if you can find a reference, follow it. If there's a
  //reference there's a cycle, that's where it starts
  template <class T>
  maybe_shared_ptr<node<T>> cycle_start (list<T> l) {
    if (empty (l)) 
      return none<node<T>::shared_ptr>();
    if (node<T>::weak_ptr* p=boost::get<node<T>::weak_ptr>(&l))
      return some (p->lock());

    return cycle_start (tl (l));
  }

  //Number of 'tl' invocations needed to reach 'x'
  template <class T>
  int distance (int acc, typename node<T>::shared_ptr x, ptr_t<node<T>> l) {
    //'l' may not be empty
    if (empty (l))
      throw std::runtime_error ("distance_aux : empty list");
    //'x' must be reachable from l without following a reference
    if (boost::get<node<T>::weak_ptr> (&l))
      throw std::runtime_error ("distance_aux : cycle encountered");
    typename node<T>::shared_ptr p =boost::get<typename node<T>::shared_ptr>(l);
    if (p == x)
      return acc;

    return distance (acc + 1, x, tl (l));
  }

  //Invoke 'tl', 'n' times
  template <class T>
  list<T> advance (int n, list<T> l) {
    if (n == 0)
      return l;
    return advance ((n - 1), tl (l));
  }

}//namespace<anonymous>

template <class T>
list<T> copy (list<T> src){
  list<T> dst = copy_aux (src);

  //Patch up 'dst' in the case 'src' has a cycle
  maybe_shared_ptr<node<T>> maybe_cycle=cycle_start (src);
  if (!!maybe_cycle) {
    tl (last (dst)) = ref (advance (distance (0, maybe_cycle.get (), src), dst));
  }

  return dst;
}

// namespace {
//   template<class T>
//   int print_aux (int count, list<T> l) {
//     if (count > 100) return count;
//     if (!empty (l)) {
//       std::cout << hd (l);
//       if (!empty (tl (l))) std::cout << "; ";
//       return print_aux (count + 1, tl (l));
//     }
//     return count;
//   }
//
//   template <class T>
//   void print (ptr_t<node<T>> l) {
//     std::cout << "[";   
//     int count = print_aux (0, l);
//     if (count >= 100)
//       std::cout << "...";
//     else std::cout << "]";
// }
//
// }//namespace<anonymous>
//
//
// int main () {
//
//   try{
//     list<int> p = cons (1, cons (0, nil<int> ()));
//     tl (last (p)) = ref (p);
//
//     list<int> q = copy (p);
//
//     print (p); std::cout << std::endl;
//     print (q); std::cout << std::endl;
//   }
//   catch (std::runtime_error const& e) {
//     std::cerr << e.what () << std::endl;
//   }
//
//   return 0;
// }

// --

struct E_let; //let f = e
struct E_letrec; //let rec f = (fun x -> e (f))
struct E_fun;
struct E_constant;
struct E_variable;
struct E_addition;
struct E_subtraction;
struct E_multiplication;
struct E_division;
struct E_apply; //f x
struct E_if;

typedef boost::variant<
    E_constant
  , E_variable
  , boost::recursive_wrapper<E_addition>
  , boost::recursive_wrapper<E_subtraction>
  , boost::recursive_wrapper<E_multiplication>
  , boost::recursive_wrapper<E_division>
  , boost::recursive_wrapper<E_let>
  , boost::recursive_wrapper<E_letrec>
  , boost::recursive_wrapper<E_fun>
  , boost::recursive_wrapper<E_apply>
  > expression_t;

struct E_constant { double val; };
struct E_variable { std::string val; };
struct E_fun { std::string arg; expression_t ast; };
struct E_let { std::string tag; expression_t ast; };
struct E_letrec { std::string tag; expression_t ast; };
struct E_addition { expression_t left; expression_t right; };
struct E_subtraction { expression_t left; expression_t right; };
struct E_multiplication { expression_t left; expression_t right; };
struct E_division { expression_t left; expression_t right; };
struct E_apply { expression_t func; expression_t arg; };

std::string string_of_expression (expression_t);
namespace {
  struct string_of_expression_visitor {
    typedef std::string result_type;
    std::string operator ()(E_constant e) const {
      std::ostringstream os;
      os << "E_constant " << e.val;
      return os.str ();
    }
    std::string operator ()(E_variable e) const {
      std::ostringstream os;
      os << "E_variable \"" << e.val <<"\"";
      return os.str ();
    }
    std::string operator ()(E_fun e) const {
      std::ostringstream os;
      os << "E_fun (" << e.arg << ", " << string_of_expression (e.ast) << ")";
      return os.str ();
    }
    std::string operator ()(E_let e) const {
      std::ostringstream os;
      os << "E_let (\"" << e.tag << "\", " << string_of_expression (e.ast) << ")";
      return os.str ();
    }
    std::string operator ()(E_letrec e) const {
      std::ostringstream os;
      os << "E_letrec (\"" << e.tag << "\", " << string_of_expression (e.ast) << ")";
      return os.str ();
    }
    std::string operator ()(E_addition e) const {
      std::ostringstream os;
      os << "E_addition (" << string_of_expression (e.left) << ", " << string_of_expression (e.right) << ")";
      return os.str ();
    }
    std::string operator ()(E_subtraction e) const {
      std::ostringstream os;
      os << "E_subtraction (" << string_of_expression (e.left) << ", " << string_of_expression (e.right) << ")";
      return os.str ();
    }
    std::string operator ()(E_multiplication e) const {
      std::ostringstream os;
      os << "E_multiplication (" << string_of_expression (e.left) << ", " << string_of_expression (e.right) << ")";
      return os.str ();
    }
    std::string operator ()(E_division e) const {
      std::ostringstream os;
      os << "E_division (" << string_of_expression (e.left) << ", " << string_of_expression (e.right) << ")";
      return os.str ();
    }
    std::string operator ()(E_apply e) const {
      std::ostringstream os;
      os << "E_apply (" << string_of_expression (e.func) << ", " << string_of_expression (e.arg) << ")";
      return os.str ();
    }
  };
}//namespace<anonymous>

std::string string_of_expression (expression_t e) {
  return boost::apply_visitor (string_of_expression_visitor (), e);
}

struct V_float;
struct V_closure;

typedef boost::variant<
    V_float
  , V_closure //boost::recursive_wrapper<V_closure>
  > value_t;

typedef std::pair<std::string, value_t> p_t;
typedef list<p_t> env_t;

struct V_float { double val; };

struct V_closure {
  env_t env;
  expression_t e;
};

std::string string_of_environment (env_t const& env);

namespace {
  struct string_of_value_visitor {
    typedef std::string result_type;

    std::string operator ()(V_float v) const {
      std::ostringstream os;
      os << "V_float " << v.val;
      return os.str ();
    }

    std::string operator ()(V_closure v) const {
      std::ostringstream os;
      os << "V_closure (";
      os << (is_ref (v.env) ? "[ ... ]" : string_of_environment (v.env));
      os << ", " << string_of_expression (v.e);
      os << ")";
      return os.str ();
    }
  };

}//namespace<anonymous>

std::string string_of_value (value_t v) {
  return boost::apply_visitor (string_of_value_visitor (), v);
}

namespace {
  std::string string_of_environment_aux (env_t const& l) {
    if (empty (l))
      return "";

    p_t const& p = hd (l);
    std::ostringstream os;
    os << "(\"" << p.first << "\", ";
    os << string_of_value (p.second) << ")";
    if (!empty (tl (l))) os << "; ";

    return os.str () + string_of_environment_aux (tl (l));
  }

}//namespace<anonymous>

std::string string_of_environment (env_t const& l) {
  return "[" + string_of_environment_aux (l)+"]";
}

value_t eval (env_t* env, expression_t const& ast);

struct eval_visitor {

  typedef value_t result_type;
  env_t* env;
 
  eval_visitor (env_t* env) : env (env) {}
  
  value_t operator () (E_constant const& e) const {
    return V_float {e.val};
  }

  value_t operator () (E_variable const& xpr) const {
    try{
      return assoc (xpr.val, *env);
    }
    catch (not_found const&) {
      throw std::runtime_error (
         "\""+xpr.val+"\" isn't bound in the environment");
    }
  }

  value_t operator ()(E_addition const& xpr) const {
    value_t l=eval (env, xpr.left), r=eval (env, xpr.right);
    if (V_float*p=boost::get<V_float>(&l))
      if (V_float*q=boost::get<V_float>(&r))
        return V_float {p->val + q->val};
    throw std::runtime_error ("Bad types for operator '+'");
  }

  value_t operator ()(E_subtraction const& xpr) const {
    value_t l=eval (env, xpr.left), r=eval (env, xpr.right);
    if (V_float*p=boost::get<V_float>(&l))
      if (V_float*q=boost::get<V_float>(&r))
        return V_float {p->val - q->val};
    throw std::runtime_error ("Bad types for operator '-'");
  }

  value_t operator ()(E_multiplication const& xpr) const {
    value_t l=eval (env, xpr.left), r=eval (env, xpr.right);
    if (V_float*p=boost::get<V_float>(&l))
      if (V_float*q=boost::get<V_float>(&r))
        return V_float {p->val * q->val};
    throw std::runtime_error ("Bad types for operator '*'");
  }

  value_t operator ()(E_division const& xpr) const {
    value_t l=eval (env, xpr.left), r=eval (env, xpr.right);
    if (V_float*p=boost::get<V_float>(&l)){
      if (V_float*q=boost::get<V_float>(&r)){
        if (q->val != 0.0){
          return V_float {p->val / q->val};
        }
        else{
          throw std::runtime_error ("Attempted division by zero");
        }
      }
    }
    throw std::runtime_error ("Bad types for operator '/'");
  }

  value_t operator () (E_fun const& xpr) const {
    return V_closure {copy (*env), xpr};
  }

  value_t operator () (E_let const& xpr) const {
    value_t rhs=eval (env, xpr.ast);
    *env = cons (std::make_pair (xpr.tag, rhs), *env);
    return rhs;
  }

  value_t operator () (E_letrec const& xpr) const {

    //In OCaml, (where 'env' is of type (string * value) list) we
    //might write: 
    //
    //  let rec vars = (tag, V_closure(vars, e))::env
    //
    //That is, 'vars' is a list with a closure at the front, the
    //closure value itself contains (an expression and) a list, the
    //contained list is 'vars' itself.

    list<p_t> vars = node<p_t>::shared_ptr(new node<p_t>);
    hd (vars) = std::make_pair (xpr.tag, V_closure {ref (vars), xpr.ast});
    tl (vars) = *env; 

    *env = vars;

    return eval (env, xpr.ast);
  }

  value_t operator () (E_apply const& xpr) const {
    value_t f=eval (env, xpr.func);
    value_t x=eval (env, xpr.arg);
    if (V_closure* p = boost::get<V_closure>(&f)){
      E_fun e = boost::get<E_fun>(p->e);
      list<p_t> h=cons (std::make_pair (e.arg, x), p->env);
      return eval (&h, e.ast);
    }

    throw std::runtime_error ("Can't apply a value that is not a function");
  }
};

value_t eval ( env_t* env, expression_t const& ast) {
  return boost::apply_visitor (eval_visitor (env), ast);
}

int main ()
{
  try {
    value_t v;
    env_t env = nil<p_t> ();

    //f = fun x -> fun y -> x * y
    eval (&env, 
          E_let {"f", 
              E_fun {"x", 
                E_fun {"y", 
                  E_multiplication {E_variable {"x"}, E_variable {"y"}}
              }}}
          );

    //g = f 3 (i.e. fun y -> 3 * y)
    v= eval (&env, 
             E_let {"g", 
                 E_apply {E_variable {"f"}, E_constant {3.0}}             
             }
          );

    //g 4
    v= eval (&env, 
             E_let {"g", 
                 E_apply {E_variable {"g"}, E_constant {4.0}}
             }
           );
          
    std::cout << string_of_value (v) << std::endl;// i.e. 12

    //std::cout << string_of_environment (env) << std::endl;
  }
  catch (not_found const&) {
    std::cout << "Not found" << std::endl;
  }
  catch (std::runtime_error const& e) {
    std::cout << e.what () << std::endl;
  }

  return 0;
}
