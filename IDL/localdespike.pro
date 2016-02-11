FUNCTION localdespike,dta,window=window,nsig=nsig,holes=holes,single=single
  t = reform(dta)
  IF n_elements(window) EQ 0 THEN window = 24*7
  IF n_elements(nsig) EQ 0 THEN nsig = 6

  IF keyword_set(single) OR n_elements(t[0,*]) EQ 1 THEN BEGIN
    tmax = make_array(window*2,n_elements(t))
    FOR i = 0,window*2-1 DO BEGIN
      tmax[i,*] = shift(t,i-window)
    ENDFOR 
    rng=amedian(stddev(tmax,dim=1,/nan)*nsig,window)
    rng1 = replicate_arr(rng,n_elements(t[0,*]))
    rng2 = rng1 * (-1)
    rng_mn = replicate_arr(amedian(mean(tmax,dim=1,/nan),window),n_elements(t[0,*]))
    rng1 += rng_mn
    rng2 += rng_mn
  ENDIF ELSE BEGIN 
    rng=amedian(stddev(t,dim=2,/nan)*nsig,window)
    rng_mn = amedian(mean(t,dim=2,/nan)*nsig,window)
    rng1 = replicate_arr(rng,n_elements(t[0,*]))
    rng2 = rng1 * (-1)
    rng_mn = replicate_arr(amedian(mean(t,dim=2,/nan)*nsig,window),n_elements(t[0,*]))
    rng1 += rng_mn
    rng2 += rng_mn
  ENDELSE 
  tt = t
  b1 = where(t GT rng1 AND finite(t),nbv1)
  IF nbv1 GT 0 THEN tt[b1] = nan()
  b2 = where(t LT rng2 AND finite(t),nbv2)
  IF nbv2 GT 0 THEN tt[b2] = nan()
  holes = [b1,b2]
  return,tt
END
