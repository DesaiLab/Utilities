FUNCTION expand_arr,vals,factor
;e.g., turn 0,1,2 into 0,0,1,1,2,2 
  nvals = n_elements(vals)
  fx = long(factor)
  outarr = make_array(nvals*fx,type=size(vals,/type),/nozero)
  FOR i = 0l,nvals-1 DO outarr[(i*fx):((i*fx)+(fx-1l))] = vals[i]
  return,outarr
END
