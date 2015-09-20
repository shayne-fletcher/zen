Describing finanical contracts requires the ability to manipulate symbolic payoff expressions dependent on observations of stochastic processes such as, the price of an equity or the value obtained by the interest rate index LIBOR.

To do this requires developing an algebra of real numbers with "placeholders" where a placeholder represents a stochastic process. 

For example, the formula, 

  notional * max (market ("IBM US Equity") - strike, 0.0)

describes a call option payoff. Here, 'notional' and 'strike' we'll assume are fixed contractual constants but 'market ("IBM")', an indeterminate in the expression, is a placeholder.

For the above formula to result in a real payment amount, it is necessary for the contract to specify the observation date on which the IBM asset price is to be observed, another contractual detail e.g.

  fix (2015-09-17, notional * max (market "IBM" - strike, 0.0))

The above is equivalent of course to
  
   notional * max (fix (2015-09-17, market "IBM") - strike, 0.0)

The goal of this laboratory is to implement this small language of arithmetic expressions with placeholders. The following psuedo-code should get you going:

  type expression =
     | Const of float
     | Market of string
     | Fixed of expression
     | Add of expression * expression
     .
     .
     .

  let market (ticker : string) = ...

  let ( + ) (a : expression) (b : expression) = ...

  ...

The goal is to be able to write symbolic "payoffs" such as

  let s = fix ("2015-09-30", (market ("IBM US Equity") / fix ("2015-09-17", market ("IBM US Equity")) - const (1.0)))

You should also provide a function 'simplify'

  val simplify : (string * string * float) list -> expression -> expression

The idea of 'simplify' is to substitute from a list of fixings into an expression to produce the resulting "simplified" expression. For example,

  let ticker = "IBM US Equity" in
  simplify [(ticker, "2015-09-30", 0.12);(ticker, "2015-09-17", 0.1)] s

should produce the expression `const 0.2`.

