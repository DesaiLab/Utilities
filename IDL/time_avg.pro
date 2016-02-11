FUNCTION time_avg,arr,col,newcol=newcol,sd=sd
  ;arr is cols x row, average where uniq in col

  colarr = arr[col,*]
  uniqcol = uniq(colarr,sort(colarr))
  newcol = reform(colarr[uniqcol])
  s = size(arr)
  cols = s[1]
  rows = s[2]
  tp = s[3]
  newrows = n_elements(newcol)
  newarr = make_array(cols,newrows,type=tp)
  FOR r = 0,newrows-1 DO BEGIN
    g = where(colarr EQ newcol[r],numg)
    IF numg GT 0 THEN BEGIN
      FOR c = 0,cols-1 DO BEGIN
        IF keyword_Set(sd) THEN newarr[c,r] = stddev(arr[c,g],/nan) ELSE newarr[c,r] = mean(arr[c,g],/nan)
      ENDFOR
    ENDIF ELSE newarr[*,r] = nan()
  ENDFOR

  return,newarr

END
