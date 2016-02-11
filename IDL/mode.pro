FUNCTION mode,arr
;return most common element of an array
  wg = where(isnotnan(arr),nwg)
  IF nwg GT 0 THEN BEGIN
    garr = arr[wg]
    his = histogram(garr,binsize=1,omin=om)
    outval =  ((where(his EQ max(his)))[0]) + om
    return,outval
  ENDIF ELSE return,nan()
END
