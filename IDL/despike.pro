FUNCTION despike,data,nsig=nsig,dmean=dmean,trend=trend,badval=badval,ddata=ddata,hnew=hnew

  ddata = detrend(data,dmean=dmean,trend=trend,badval=badval)

  hold=-1
  hnew=0

  IF NOT keyword_set(nsig) THEN nsig = 6
  IF keyword_set(badval) THEN BEGIN
    dmean = badval
    trend = badval
    outdata = data
    g = where(ddata NE badval,num)
  ENDIF ELSE BEGIN
    dmean = nan()
    trend = nan()
    outdata = data
    g = where(finite(ddata),num)
  ENDELSE

  IF num GT 1 THEN BEGIN
    sig=stdev(ddata[g])
    h=where(abs(ddata) gt nsig*sig,hnew)
    IF hnew GT 0 THEN BEGIN
      IF keyword_set(badval) THEN outdata[h] = badval ELSE outdata[h] = nan()
    ENDIF
  ENDIF

  return,outdata

END
