FUNCTION DMS_to_Decimal,d,m,s,double=double
  IF keyword_set(double) THEN BEGIN
    d1 = double(d)
    m1 = double(m)
    s1 = double(s)
  ENDIF ELSE BEGIN
    d1 = d
    m1 = m
    s1 = s
  ENDELSE
  decimal = d1 + (m1 / 60.0) + (s1 / 3600.0)
  return,decimal
END
