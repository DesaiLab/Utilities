FUNCTION myfit,x,y,degree=degree,pvalue=pvalue,corr=corr,badval=badval,ngvals=ngvals,lad=lad,_extra=ex,yfit=yfit,sigma=sigma,status=status
  xx = reform(x)
  yy = reform(y)
  nan = !values.f_nan
  IF n_elements(badval) NE 0 THEN BEGIN
    badx = where((xx EQ badval),nbadx)
    bady = where((yy EQ badval),nbady)
    IF nbadx GT 0 THEN xx[badx] = nan
    IF nbady GT 0 THEN yy[bady] = nan
  ENDIF 
  gval = where(finite(xx) AND finite(yy),ngvals)
  IF n_elements(degree) EQ 0 THEN degree=1
  IF ngvals GE (degree+1) THEN BEGIN
    xx = xx[gval]
    yy = yy[gval]
    yfit = replicate(nan(),ngvals)
    IF keyword_set(lad) THEN fit = ladfit(xx,yy,_extra=ex) ELSE IF stddev(xx) GT 0 THEN fit=poly_fit(xx,yy,degree,yfit=yfit,_extra=ex,sigma=sigma,status=status) ELSE fit = [nan(),nan()]
    IF keyword_set(lad) THEN yfit = fit[0] + (xx * fit[1])
    corr = correl(yy,yfit)
    IF ngvals GT 2 THEN pvalue = corr_ttest(corr=abs(corr),N=ngvals) ELSE pvalue = nan()
  ENDIF ELSE BEGIN 
    corr = nan
    pvalue = nan
    fit = replicate(nan,n_elements(degree)+1)
  ENDELSE 
  return,reform(fit)
END
