FUNCTION replicate_arr,arr,times
;concatenate vector arr to itself
  tm = long(times)
  nl = n_elements(arr)
  outarr = make_array(nl*tm,type=size(arr,/type),/nozero)
  FOR i = 0l,tm-1l DO outarr[(i*nl):((i*nl)+(nl-1l))] = arr
  return,outarr
END
