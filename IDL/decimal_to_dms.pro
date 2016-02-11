FUNCTION Decimal_to_DMS,decimal,double=double

  d = float(fix(decimal))
  m1 = double((decimal - d) * 60.0)
  m = float(fix(m1))
  s = double((m1-m)*60.0)
  IF NOT keyword_set(double) THEN s = float(s)
 
  IF n_elements(decimal) GT 1 THEN return,[transpose(d),transpose(m),transpose(s)] ELSE return,[d,m,s]

END
