//"c:/program files (x86)/Microsoft Visual Studio 12.0/vc/vcvarsall.bat" x64
//cl /Feassociation_list.exe /Zi /MDd /EHsc /I d:/boost_1_55_0 association_list.cpp

#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>

#include <list>
#include <iterator>
#include <algorithm>
#include <string>
#include <iostream>

struct not_found {};

//val assoc : 'a -> ('a * 'b) list -> 'b 
//
//[assoc a l] returns the value
//associated with key [a] in the list of pairs [l]. That is, [assoc a
//[...; (a, b); ...] = b] if [(a, b)] is the leftmost binding of [a]
//in [l]. Raise [Not_found] if there is no value associated with [a]
//in [l].

template <class A, class B>
B assoc (A a, std::list<std::pair<A, B>> const& l) {
  std::list<std::pair<A, B>>::const_iterator where=
    std::find_if (std::begin (l), std::end (l), 
       [=] (std::pair<A, B> const& p) -> bool { return p.first == a; }
   );

  if (where == std::end (l)) 
  throw not_found ();

  return where->second;
}

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
struct E_let_rec { std::string tag; expression_t ast /*E_fun*/; };

struct V_float;
struct V_closure;

typedef boost::variant<
    V_float
  , V_closure//boost::recursive_wrapper<V_closure>
  > value_t;

typedef std::list<std::pair<std::string, value_t>> env_t;

struct V_float { double val; };

//   | E_let_rec (f, e, pos) ->
//     begin
//       match f with
//         | E_var (tag, pos) ->
//           (
//             match e with
//             | E_fun (_, _, _) ->
//               let rec vars = (tag, V_closure(vars, e))::!env in
//               (env := vars ; (eval env e))
//             | _ as unk ->
//               let pos = sref_of_expression unk in
//               raise_eval_error pos "Error: A function body must follow the '='"
//           )
//         | _ -> raise_eval_error pos "Error: Bad expression following 'let'"
//     end


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

    std::cout << "start" << std::endl;

    std::string tag = xpr.tag;
    expression_t e = xpr.ast; //E_fun (_, _)
    V_closure clo {*env, e};
    clo.env.emplace_front (tag, clo);
    *env = clo.env;

    std::cout << "end" << std::endl;

    return eval (env, e);
  }

  template <class T>
  value_t operator () (T const&) const {
    throw std::runtime_error ("Not implemented");
  }
 
};

value_t eval (
     env_t* env
   , expression_t const& ast) {
  return boost::apply_visitor (eval_visitor (env), ast);
}

int main ()
{
  try
    {
      env_t env;
      env.emplace_front ("foo", V_float {1});
      env.emplace_front ("bar", V_float {2});
      env.emplace_front ("foo", V_float {3});

      eval (&env, E_let_rec {"f", E_fun {"x", E_constant {1.0}}});
      
      //std::cout << assoc (std::string("foo"), env) << std::endl;
      //std::cout << assoc (std::string("bar"), env) << std::endl;
      //std::cout << assoc (std::string("baz"), env) << std::endl;

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
