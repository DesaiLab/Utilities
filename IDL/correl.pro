FUNCTION correl,a,b,_extra=_extra
  bv = where(isnotnan(a) AND isnotnan(b),nbv)
  IF nbv GT 0 THEN return,correlate(a[bv],b[bv],_extra=_extra) ELSE return,nan()
END
