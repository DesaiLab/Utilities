FUNCTION fit_array,arrx,arry,fit=fit,rsq=rsq
  good = where(isnotnan(arrx) AND isnotnan(arry),ngood)
  IF ngood GT 1 THEN BEGIN 
    fit = ladfit(arrx[good],arry[good])
    rsq = correlate(arrx[good],arry[good])^2
    return, fit[0] + (fit[1] * arrx)
  ENDIF ELSE BEGIN
    fit = [nan(),nan()]
    rsq = nan()
    return,arrx
  ENDELSE 
END
