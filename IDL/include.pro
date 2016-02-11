FUNCTION include,lst,vals,cnt,outlist=outlist
  ;take list and include all vals

  ex = replicate(0b,n_elements(lst))
  FOR x = 0,n_elements(vals)-1 DO ex = ex OR (lst EQ vals[x])
  tmp = where(ex EQ 1,cnt)
  IF cnt GT 0 THEN outlist = lst(tmp) ELSE outlist = -1
  return,ex

END
