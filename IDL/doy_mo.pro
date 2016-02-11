FUNCTION doy_mo,mo,yr=yr
  dim = days_in_month(mo,y=yr)
  n = string(indgen(dim) + 1,format='(i2.2)')
  n = string(mo,format='(i2.2)') + n
  IF keyword_set(yr) THEN n = string(yr MOD 100,format='(i2.2)') + n ELSE n = '01' + n
  jds = [0]
  FOR i = 0,dim-1 DO BEGIN
    jd = dy_to_jd(n[i])
    jds = [jds,jd]
  ENDFOR
  jds = cdr(jds)
  return,jds
END
