FUNCTION average_int,arr,int,ntimes=ntimes,std=std,numvals=numvals
;average every int values, return an array ntimes large
  nel = n_elements(arr)
  IF n_elements(ntimes) EQ 0 THEN ntimes = long(int) > 0l < (nel-1l)
  outarr = make_array(ntimes,/float,value=nan())
  std = outarr
  numvals= outarr
  FOR i = 0l,ntimes-1l DO BEGIN
    avglocs = numgen(long(i),long(nel-1l),long(int))
    goodlocs = where(isnotnan(arr[avglocs]),numgood)
    numvals[i] = numgood
    IF numgood GE 1 THEN outarr[i] = mean(arr[avglocs],/nan)
    IF numgood GT 2 THEN std[i] = stddev(arr[avglocs],/nan) /sqrt(numgood)
  ENDFOR
  return,outarr
END
