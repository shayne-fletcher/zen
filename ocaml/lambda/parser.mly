%{
  let mk_free a = Lambda.Free a
  let mk_lambda ((b, bs), t) = Lambda.abstract_list (b :: bs, t)
  let apply_list (t0, us) = Lambda.apply_list (t0, us)
%}
%token <string> Tident
%token Tlambda Tdot Tlparen Trparen Teof
%nonassoc Tident Tdot Tlparen Trparen Teof
%start term
%type <Lambda.t> term

%%
term:
  | Tlambda id id_list Tdot term Teof { mk_lambda (($2, $3), $5) }
  | atom atom_list { apply_list ($1, $2) }
  ;
id_list:
  | /*nothing*/ { [] }
  | id id_list { $1 :: $2 }
  ;
atom_list:
  | /*nothing*/ { [] }
  | atom atom_list { $1 :: $2 }
  ;
atom:
  | id { mk_free $1 }
  | Tlparen term Trparen { $2 }
  ;
id:
  | Tident { $1 }
  ;
%%
