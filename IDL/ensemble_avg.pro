FUNCTION ensemble_avg,arrs,conf=conf
  ;average in dimenstion 3
  ;arr is cols x rows x arrs
  ;return cols x rows   - each element averaged

  s = size(arrs)
  IF n_elements(s) EQ 6 THEN BEGIN 
    rows = s[2]
    cols = s[1]
    dim = s[3]
    tp = s[4]
    outarr = make_array(cols,rows,type=tp)
    FOR r = 0,rows-1 DO BEGIN
      FOR c = 0,cols-1 DO BEGIN
        outarr[c,r] = mean(arrs[c,r,*],/nan)
      ENDFOR
    ENDFOR
    return,outarr
  ENDIF ELSE return,nan()
END
