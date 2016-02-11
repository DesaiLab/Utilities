FUNCTION shiftchop,arr,val,chop
  IF val LE 0 THEN BEGIN
    n1 = 0
    n2 = n_elements(arr)-1-abs(val)
    IF keyword_set(chop) THEN n2 = n_elements(arr)-1-abs(chop)
  ENDIF ELSE BEGIN
    n1 = val
    IF keyword_set(chop) THEN n1 = chop
    n2 = n_elements(arr)-1
  ENDELSE
  return,(shift(Arr,val))[n1:n2]
END
