FUNCTION exclude,lst,vals,cnt,outlist=outlist
  ;take list and exclude and vals

  ex = replicate(1b,n_elements(lst))
  FOR x = 0,n_elements(vals)-1 DO ex = ex AND (lst NE vals[x])
  tmp = where(ex EQ 1,cnt)
  IF cnt GT 0 THEN outlist = lst(tmp) ELSE outlist = -1
  return,ex

END
