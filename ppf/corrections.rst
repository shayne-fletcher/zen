Globally:
  - replace "unit - test" with "unit test"

Prelims
 iii Change "Shayne Fletcher and Christopher Gardner" to "S. Fletcher & C. Gardner".
 vii Change "Numerical pricing with PPF in excel" to "Numerical pricing with PPF in Excel"
 viii Change "B Boost Python" to "B Boost.Python"

Chapter 1
  1 28 Change "analysts/programmers" to "analyst/programmer"
  2  5 Change "neither the interest not time" to "neither the interest or time"
  
Chapter 2
  8 34 Change "site}" to "site"

Chapter 3
 24 27 Change "array(sum" to "array sum("
 33 30 Change "As the exact form of this linear system varies from one source to another, 
               we use the form found in" to 
              "The exact form of this linear system varies from one source to another. 
               We use the form found in"
 38 43 Change "Find the inverse of a square non-singular matrix" to
              "Find the inverse of a square non-singular matrix."
 39 16 Change "Find the pseudo-inverse of a matrix" to
              "Find the pseudo-inverse of a matrix."
 41 37 Change
        if a[i, j] <> 0.0: raise RuntimeError, "Matrix not upper
        diagonal"
       to
        if a[i, j] <> 0.0: raise RuntimeError, "Matrix not upper"
        "diagonal"
 42 24 Change
         >>> W[:n, :n] = diag(sig)
       to
         >>> W=numpy.zeros((n+1, n))
         >>> W[:n, :n] = diag(sig)
 59 47 Change "s \le t for s \le t" to "s \le t"

Chapter 6
 75 45 Change
          fixed_pay_holiday_centers = attributes["fixed-pay-holiday-
                                                 centers"]
       to 
          fixed_pay_holiday_centers = attributes["fixed-pay-holiday-"
                                                 "centers"]
 76 4 Change
          float_pay_holiday_centers = attributes["float-pay-holiday-
                                      centers"]
      to
          float_pay_holiday_centers = attributes["float-pay-holiday-"
                                                 "centers"]
 76 20 Change
                    , accrual_holiday_centers = fixed_pay_holiday_
                                                centers)
       to
                    , accrual_holiday_centers =
                        fixed_pay_holiday_centers)
 76 44 Change
                    , accrual_holiday_centers = float_pay_holiday_
                                                centers
       to
                    , accrual_holiday_centers =
                          float_pay_holiday_centers
 77 15 Change
          proj_start, proj_end, reset_accrual_dcf = \
               (obs.proj_start_date(), obs.proj_end_date(), obs.year_
               fraction())
       to
          proj_start, proj_end, reset_accrual_dcf = \
               (obs.proj_start_date(), obs.proj_end_date(),
                obs.year_fraction())
 80 20 Change
        def set_observables(self, observables): self.__observables =
        observables
       to
        def set_observables(self, observables): self.__observables =\
        observables
 81  6 Change
        , pay_shift_method  = ppf.date_time.shift_convention.modified_
                              following
       to
        , pay_shift_method  = 
           ppf.date_time.shift_convention.modified_following
 81  9 Change
        , accrual_shift_method  =  ppf.date_time.shift_convention.modified_
                                   following
       to
        , accrual_shift_method  =  
            ppf.date_time.shift_convention.modified_following
 90 36
       Change
        >>> structure = trade((pay_leg, receive_leg), (ex_sched,
                        exercise_type.callable))
       to
        >>> structure = trade((pay_leg, receive_leg), (ex_sched,\
        exercise_type.callable))

Chapter 7

 94  3 Change
        def __str__(self) :
          s = "payment [%s, %s, %s, %s], " % \
               (self.__pay_rcv, self.__leg_id, self.__reset_id, str(self._
               _flow))
          return s

       to
        def __str__(self) :
          s = "payment [%s, %s, %s, %s], " % \
               (self.__pay_rcv, self.__leg_id, self.__reset_id, 
                str(self.__flow))
          return s
 94 15 Change
          def pay_currency(self) : return self.__exercise_opportunity.fee_
                                   currency()
       to
          def pay_currency(self) :
            return self.__exercise_opportunity.fee_currency()

 97 36 Change
          self.__symbol_table[name] = (at, self.__model.state().create_
                                      variable())
       to
          self.__symbol_table[name] = \
            (at, self.__model.state().create_variable())

  98 20 Change "create an(" to "create an ("

Chapter 8

102 23 Change 
         raise RuntimeError,\
           "incremental variance is negative"+" t = "+str(t)+" T = 
           "+str(T)
       to
         raise RuntimeError,\
           "incremental variance is negative"+" t = "+str(t)+\
           " T = "+str(T)
104  5 Change "enasles" to "enables".
104 31 Change
         raise RuntimeError, 'bond maturity after terminal measure 
                              bond maturity'
       to
         raise RuntimeError,\
           'bond maturity after terminal measure bond 
            maturity'
105  1 Change
       raise RuntimeError, 'time beyond terminal measure bond maturity'
      to
       raise RuntimeError,\
         'time beyond terminal measure bond maturity'
105 22 Change
         dcf = year fraction(proj start date, proj end date, libor obs.
         proj basis())
       to
         dcf = libor_obs.year_fraction()
113  8 Change
         raise RuntimeError, \
           'expected number of simulations to be even with
            antithetic'
       to
         raise RuntimeError, \
           'expected number of simulations to be even with'\
           'antithetic'
116  7 Change
         accrual_start_days = env.relative_date(flow.accrual_
                              start_date())
       to
         accrual_start_days = env.relative_date(
           flow.accrual_start_date())
116 34 Remove the '\' character.
118 12 "model" should not be in bold-face.
118 28 Change
          raise RuntimeError, "either the 'rollback' or 'evolve' must be
                               defined"
       to
          raise RuntimeError, \
             "either the 'rollback' or 'evolve' must be defined"
118 30 Change
          raise RuntimeError, "either the 'rollback' or 'evolve' must be
                               defined"
       to
          raise RuntimeError, \
            "either the 'rollback' or 'evolve' must be defined"
118 35 Change

          raise RuntimeError, "the 'exercise' cannot be 
                               bound to the 'rollback'"
       to
          raise RuntimeError, \
            "the 'exercise' cannot be bound to the 'rollback'"

121 29 Change
          if model args <> None and model args.has key("explanatory
                           variables leg id")
       to
          if model_args <> None and \
            model_args.has_key("explanatory variables leg id"):

Chapter 9

124  9 Replace "$T_i <= t_k ~\forall k >= n(T_i)$" 
      with "$T_i \le t_k ~\forall k \ge n(T_i)$" 
     (i.e. proper lesser equal, greater equal signs).
124 41 Remove the '\' character.
125  1 Remove the '\' character.
125 31 Change "set, up" to "set up,"
127 39 Change 
          # rollback any symbols in symbol table not already
            rolled back
       to
          # rollback any symbols in symbol table not already
          # rolled back
128 34 Change
        self.__symbols.append( model.rollback().rollback_max(0.0, t, 
                              state\, requestor, env, value).mean())
       to
         self.__symbols.append(
           model.rollback().rollback_max(
             0.0, t, state, requestor, env, value).mean())
131 38 Change "of structure simply" to "of a structure simply".
132 23 Change "purpose of provided" to "purpose of providing".
133 47 Change
         # max for numpy arrays
           max = numpy.vectorize(lambda x, y: (x, y) [x < y])
       to
         # max for numpy arrays
         max = numpy.vectorize(lambda x, y: (x, y) [x < y])
134 15 Change
         raise RuntimeError, "'explanatory variables' array has
         wrong size"
       to
         raise RuntimeError, \
           "'explanatory variables' array has wrong size"
135 45 Change
         raise RuntimeError, "attempting to call 'numeraire'
         in the past"
       to
         raise RuntimeError, \
           "attempting to call numeraire in the past"
136  7 Change
         raise RuntimeError, "attempting to call 'explanatory_
                              variables' in the past"
       to
         raise RuntimeError, \
           "attempting to call 'explanatory_variables' in the past"
136 39 Change
          if self. trade.has_exercise schedule() and self._regression_model
          == None:
       to
          if self. trade.has_exercise schedule() \
              and self._regression_model == None:
136 41 Change
         raise RuntimeError, "exercise schedule present but no
                              'regression model'"
       to
         raise RuntimeError, \
           "exercise schedule present but no 'regression model'"
137  8 Change
         num_expl_vars = self.__regression_model.exercise().num_explanatory_
                         variables()
       to
         num_expl_vars = \
           self.__regression_model.exercise().num_explanatory_variables()
141 31 Change
          self.__exercise_helper.update_indicator(to_, vs, self.__
                                                 fitted_fos[ex_cnt])
       to
          self.__exercise_helper.update_indicator(
            to_, vs, self.__fitted_fos[ex_cnt])
141 34 Change
          berm = self.__exercise_helper.max(to_, self.__trade.exercise_
                                           type()*underlying, berm)
       to
          berm = self.__exercise_helper.max(
            to_, self.__trade.exercise_type()*underlying, berm)

Chapter 10
147 29 Remove the '\' character.
149 22 Change
        expected = _fixed_leg_pv(fixed_leg, env)+_funding_leg_pv
                   (funding_leg, env)
       to
        expected = _fixed_leg_pv(fixed_leg, env)+\
                   _funding_leg_pv(funding_leg, env)
151 4 Change
        model_args = {"num sims": 6000, "seed": 1234, "explanatory 
                      variables leg id": 0} 
      to
        model_args = {"num sims": 6000, "seed": 1234
                    , "explanatory variables leg id": 0} 
153 29 Change
         # lookup 'floor', 'fixed_rate', 'leverage', 'target', 
         'redemption_floor'
         # and 'redemption_cap'
       to
         # lookup 'floor', 'fixed_rate', 'leverage', 'target', 
         # 'redemption_floor'and 'redemption_cap'
154 38 Change
        cpn = flow.notional()*flow.year_fraction()*(controller.libor
        (t, state) \ +spread)*controller.pay_df(t, state)
       to
        cpn = flow.notional()*flow.year_fraction()*(\
          controller.libor(t, state) +spread)*\
          controller.pay_df(t, state)
155 21 Change
         tarn = ppf.core.trade((funding_leg, coupon_leg)) # note the 
                ordering
       to
         tarn = ppf.core.trade((funding_leg, coupon_leg)) # note the 
                                                          # ordering
155 24 Change
        symbol_value_pairs_to_add.append(("accrued_coupon", numpy.
                                          zeros(5000)))
        symbol_value_pairs_to_add.append(("target_indicator", numpy.
                                          zeros(5000)))
       to
         symbol_value_pairs_to_add.append(
           ("accrued_coupon", numpy.zeros(5000)))
         symbol_value_pairs_to_add.append(
           ("target_indicator", numpy.zeros(5000)))
156  9 Change
         tarn = ppf.core.trade((funding_leg, coupon_leg)) # note the 
                ordering
       to
         tarn = ppf.core.trade((funding_leg, coupon_leg)) # note the 
                                                          # ordering
156 12 Change
        symbol_value_pairs_to_add.append(("accrued_coupon", numpy.
          zeros(5000)))
        symbol_value_pairs_to_add.append(("target_indicator", numpy.
          zeros(5000)))
       to
         symbol_value_pairs_to_add.append(
           ("accrued_coupon", numpy.zeros(5000)))
         symbol_value_pairs_to_add.append(
           ("target_indicator", numpy.zeros(5000)))
156 38 Change
         tarn = ppf.core.trade((funding_leg, coupon_leg)) # note the 
                ordering
       to
         tarn = ppf.core.trade((funding_leg, coupon_leg)) # note the 
                                                          # ordering
156 41 Change
        symbol_value_pairs_to_add.append(("accrued_coupon", numpy.
          zeros-(5000)))
        symbol_value_pairs_to_add.append(("target_indicator", numpy.
          zeros(5000)))
       to
         symbol_value_pairs_to_add.append(
           ("accrued_coupon", numpy.zeros(5000)))
         symbol_value_pairs_to_add.append(
           ("target_indicator", numpy.zeros(5000)))

Chapter 11
160  9 Change 
         "class boost:: gregorian::
         date"
       to "class boost::gregorian::date" (spurious space and can't 
       line break there).

Chapter 12
166 36 Change "Python exceptions (In fact, the code" to
       "Python exceptions (in fact, the code"
166 40 Change "with a custom error code." to "with a custom error code)."
168 17 Change
         Dim Pricer As Object: Set Pricer = CreateObject
                               ("ppf.black_scholes")
       to
         Dim Pricer As Object
         Set Pricer = CreateObject("ppf.black_scholes")
168 19 Change
          PPF_BlackScholes = Pricer.OptionPrice(Spot, Strike, Rate, Vol, T,
                             CallPut)
       to
          PPF_BlackScholes = Pricer.OptionPrice( _
            Spot, Strike, Rate, Vol, T, CallPut)
172 40 Change "unicode" to "Unicode".

Appendix 1

195 39 Change
        >>> print "%d, %d.%s'' % (x, y, z)
       to
        >>> print "%d,%d,%s" % (x, y, z)
196 42 Change "associated arrays" to "associative arrays".

Appendix 2

207  4 Change "Boost Python" to "Boost.Python"
213  3 Change "with_custodian_and_ward<1, 2 informs" to "with_custodian_and_ward<1, 2> informs"

Appendix 3

Bibliography

221 21 Replace "Jacckel" with "Jackel" where the a has a diacritic -- two dots (..) above it (see http://www.jaeckel.org/ for example).