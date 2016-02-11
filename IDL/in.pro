FUNCTION in,arr,val,loc=loc
;returns true if val is in arr false otherwise
  IF n_elements(arr) EQ 1 THEN BEGIN 
    retval = (arr EQ val) 
    loc = 0l
  ENDIF ELSE BEGIN
    retval = bytarr(n_elements(val))
    IF arg_present(loc) THEN loc  = lonarr(n_elements(val))
    FOR rv = 0l,n_elements(val)-1l DO BEGIN 
      wherearr = where(arr EQ val[rv],cnt)
      IF arg_present(loc) THEN loc[rv] = wherearr[0]
      IF cnt GT 0 THEN retval[rv] = 1b ELSE retval[rv] = 0b
    ENDFOR 
    IF n_elements(retval) EQ 1 THEN retval = retval[0]
    IF n_elements(loc) EQ 1 THEN loc = loc[0]
  ENDELSE
  return,retval
END
