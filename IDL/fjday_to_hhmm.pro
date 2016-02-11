FUNCTION fjday_to_hhmm,fjday,h=h,m=m,s=s,doy=doy
  fj = double(fjday)
  doy = fix(fj)
  frac = fj - double(doy)
  h1 = frac * 24.0d
  h = fix(h1)
  wh = where(h GT 23,num)
  IF num GT 0 THEN doy[wh] = doy[wh] + 1
  h = h MOD 24
  m1 = ((h1-double(h)) * 60.0d)
  m = fix(m1)
  wm = where(m GT 59,num)
  IF num GT 0 THEN h[wm] = h[wm] + 1
  m = m MOD 60
  s = float((m1-double(m)) * 60.0d)
  ws = where(s GT 59,num)
  IF num GT 0 THEN m[ws] = m[ws] + 1
  s = s MOD 60
  wm = where(m GT 59,num)
  IF num GT 0 THEN h[wm] = h[wm] + 1
  m = m MOD 60
  wh = where(h GT 23,num)
  IF num GT 0 THEN doy[wh] = doy[wh] + 1
  h = h MOD 24
  return,hm_to_hhmm(h,m)
END
