FUNCTION nan,double=double
  IF keyword_set(double) THEN return,!values.d_nan ELSE return,!values.f_nan
END
