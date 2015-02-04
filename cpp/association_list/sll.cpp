//cl /Fesll.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 sll.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

#include <stdexcept>
#include <functional>

template <class T> 
using ptr_t = 
  boost::variant <
    boost::shared_ptr<T>
  , boost::weak_ptr<T>>;

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

template <class T> struct node;

template <class T>
struct node {
  T data;
  ptr_t<node<T>> next;
  typedef boost::weak_ptr<node<T>> weak_ptr;
  typedef boost::shared_ptr<node<T>> shared_ptr;
};

template <class T> using list = ptr_t<node<T>>;

template <class T> list<T> nil (){ 
  return typename node<T>::shared_ptr (); 
}

template <class T> bool empty (list<T> l) { 
  return (get (l)) == nullptr; 
}

template <class T>
list<T> cons (T val, list<T> l) {
  return node<T>::shared_ptr (new node<T>{val, l});
}

template <class T> 
T hd (list<T> l) {   
  if (empty (l))
    throw std::runtime_error ("hd");
  return get (l) -> data;
}

template <class T> list<T>& tl (list<T> l) { 
  return get (l) -> next; 
}

template <class T>
list<T> last (list<T> l)
{
  if (empty (l))
    throw std::runtime_error ("last");

  list<T> t = tl (l);
  if (empty (t)) return l;

  return last (t);
}

template <class T>
typename node<T>::weak_ptr ref (list<T> src)  {
  return typename node<T>::weak_ptr(
           boost::get<typename node<T>::shared_ptr>(src));
}

namespace {
  template<class T>
  int print_aux (int count, list<T> l) {
    if (count > 100) return count;
    if (!empty (l)) {
      std::cout << hd (l);
      if (!empty (tl (l))) std::cout << "; ";
      return print_aux (count + 1, tl (l));
    }
    return count;
  }
}//namespace<anonymous>

template <class T>
void print (ptr_t<node<T>> l) {
  std::cout << "[";   
  int count = print_aux (0, l);
  if (count >= 100)
    std::cout << "...";
  else std::cout << "]";
}

int main () {

  list<int> p = cons (1, cons (0, nil<int> ()));
  tl (last (p)) = ref (p);

  print (p);

  return 0;
}


/*
struct E_let_rec; //let rec f x = e
struct E_fun;
struct E_constant;
struct E_variable;

typedef boost::variant<
    E_constant
  , E_variable
  , boost::recursive_wrapper<E_let_rec>
  , boost::recursive_wrapper<E_fun>
  > expression_t;

struct E_constant { double val; };
struct E_variable { std::string val; };
struct E_fun { std::string arg; expression_t ast; };
struct E_let_rec { std::string tag; expression_t ast }; //E_fun };

struct V_float;
struct V_closure;

typedef boost::variant<
    V_float
  , V_closure //boost::recursive_wrapper<V_closure>
  > value_t;

typedef std::pair<std::string, value_t> p_t;
typedef node<p_t>* env_t;

struct V_float { double val; };

struct V_closure {
  env_t env;
  expression_t e;
};

value_t eval (env_t* env, expression_t const& ast);

struct eval_visitor {

  typedef value_t result_type;
  env_t* env;
 
   eval_visitor (env_t* env) : env (env) {}
  
  value_t operator () (E_let_rec const& xpr) const {

    //In OCaml, (where 'env' is of type (string * value) list) we
    //might write: 
    //
    //  let rec vars = (tag, V_closure(vars, e))::env
    //
    //That is, 'vars' is a list with a closure at the front, the
    //closure value itselft contains (an expression and) a list, the
    //contained list is 'vars' itself.

    env_t n=new node<p_t>;
    n->next = copy (*env);
    env_t vars = n;
    vars->data = std::make_pair (xpr.tag, V_closure {vars, xpr.ast});

    *env=copy (vars);

    return eval (env, xpr.ast);
  }

  template <class T>
  value_t operator () (T const& xpr) const {
    throw std::runtime_error ("not implemented");
  }

};

value_t eval (
     env_t* env
   , expression_t const& ast) {
  return boost::apply_visitor (eval_visitor (env), ast);
}

struct not_found {}; //for 'assoc', not written yet

int test_01 ()
{
  try {
      env_t env = nullptr;
      env = cons (std::make_pair (std::string ("foo"), value_t (V_float {1})), env);
      env = cons (std::make_pair (std::string ("bar"), value_t (V_float {2})), env);
      env = cons (std::make_pair (std::string ("foo"), value_t (V_float {3})), env);

      eval (&env, E_let_rec {"f", E_fun {"x", E_constant {1.0}}});
    }
  catch (not_found const&)
    {
      std::cout << "Not found" << std::endl;
    }
  catch (std::runtime_error const& e)
    {
      std::cout << e.what () << std::endl;
    }

  return 0;
}
*/
