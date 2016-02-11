FUNCTION zapbadval,arr,locs,badvalue=badvalue,x=x,quadratic=quadratic,spline=spline,lsquadratic=lsquadratic,noedge=noedge
;extrapolates values into NAN holes
;array x values are assumed to be monotonically increasing
;unless keyword x is set (x can have no holes)
;prevent extrapolation past edges

  IF keyword_set(badvalue) THEN BEGIN
    locs = where(arr EQ badvalue,nbad,complement=good)
  ENDIF ELSE BEGIN
    badvalue = nan()
    locs = where(isnan(arr),nbad,complement=good)
  ENDELSE

  n = n_elements(arr)
  IF NOT keyword_set(x) THEN BEGIN
    x = lindgen(n)
  ENDIF

  IF n_elements(good) LE 1 THEN return,arr ELSE BEGIN
    IF nbad EQ 0 THEN return,arr ELSE BEGIN
      fixedholes = interpol(arr[good],x[good],x[locs],spline=spline,quadratic=quadratic,lsquadratic=lsquadratic)
      newarr = arr
      newarr[locs] = fixedholes[*]
      IF NOT keyword_set(noedge) THEN BEGIN 
        IF (good[0] NE 0) THEN BEGIN
          newarr[0:good[0]-1] = newarr[good[0]]
        ENDIF
        IF (good[n_elements(good)-1] NE n_elements(arr)-1) THEN BEGIN 
          newarr[good[n_elements(good)-1]+1:n_elements(arr)-1] = newarr[good[n_elements(good)-1]]
        ENDIF
      ENDIF
      return,newarr
    ENDELSE

  ENDELSE

END
