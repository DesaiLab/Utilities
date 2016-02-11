FUNCTION getnextdaydate,d,m,y
  dom = days_in_month(m,y=y)
  dp1 = d + 1
  mp1 = m
  IF keyword_set(y) THEN yp1 = y
  IF dp1 GT dom THEN BEGIN
    mp1 = m + 1
    IF mp1 GT 12 THEN BEGIN
      mp1 = 1
      IF keyword_set(y) THEN yp1 = y + 1
    ENDIF
  ENDIF
  IF keyword_set(yp1) THEN return,[dp1,mp1,yp1] ELSE return,[dp1,mp1]
END
