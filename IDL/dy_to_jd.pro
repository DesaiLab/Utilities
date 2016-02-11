FUNCTION DY_to_JD, dt

  IF mean(strlen(dt)) EQ 8 THEN BEGIN
    yr = fix(strmid(dt,0,4))
    mo = fix(strmid(dt,4,2))
    dy = fix(Strmid(dt,6,2))
  ENDIF ELSE begin
    yr = fix(strmid(dt,0,2))
    mo = fix(strmid(dt,2,2))
    dy = fix(strmid(dt,4,2))
  ENDELSE
  md = [31,28,31,30,31,30,31,31,30,31,30,31]
;  IF isleap(yr) THEN md[1] = 29
  modcum = fix(total(md,/cumulative))
  modcum = modcum - md
  md[1] = 29
  modcumleap = fix(total(md,/cum))
  modcumleap = modcumleap - md
  jd = modcum[mo-1]
  whereleap = where(isleap(yr),nleap)
  IF nleap GT 0 THEN jd[whereleap] = modcumleap[mo[whereleap]-1]
  jd = jd + dy
  return, jd
END
