FUNCTION Closest,arr,val
  ;take arr and find closest value to val
  wt = lonarr(n_elements(val))
  FOR x = 0,n_elements(val)-1 DO begin
    temparr = abs(arr-val[x])
    wheretemp = where(temparr EQ min(temparr),cnt)
    IF cnt GT 0 THEN wt[x] = wheretemp[0] ELSE wt[x] = -1
  ENDFOR
  IF n_elements(val) EQ 1 THEN return,wt[0] ELSE return,wt

END
