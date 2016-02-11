FUNCTION detrend,data,dmean=dmean,trend=trend,badval=badval

  IF keyword_set(badval) THEN BEGIN
    dmean = badval
    trend = badval
    ddata = badval
    g = where(data NE badval,num)
  ENDIF ELSE BEGIN
    dmean = nan()
    trend = nan()
    ddata = nan()
    g = where(finite(data),num)
  ENDELSE

  IF num GT 0 THEN BEGIN
    dtim = (findgen(n_elements(data)))[g]
    ddata = data[g]
    num = n_elements(ddata)
    tsum = total(dtim)
    dsum = total(ddata)
    t2sum = total(dtim^2.)
    dtsum = total(ddata*dtim)
    del = num*t2sum-tsum^2.
    a=(t2sum*dsum-tsum*dtsum)/del
    trend=(num*dtsum-tsum*dsum)/del
    dmean=dsum/num
    outdata = data
    outdata[g]=data[g]-(a+trend*dtim)
  ENDIF ELSE outdata = data

  return,outdata

END
