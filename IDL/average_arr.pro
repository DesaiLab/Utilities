FUNCTION average_arr_sdmd,arr,int,nan=nan,double=double,sz=sz,md=md,sd=sd,percentile=percentile
;simple function to average an array every int values
;array is assumed to be 1d list or column
  IF NOT keyword_set(sz) THEN sz = size(arr)
  nelem = sz[sz[0]]
  newnelem = long(nelem / int)
  IF keyword_set(double) THEN type = 5 ELSE type = 4
  outarr = make_array(newnelem,type=type)
  FOR i = 0l,newnelem-1l DO BEGIN
    CASE 1 OF 
      keyword_set(md) : outarr[i] = mode(arr[i*int:(i*int)+(int-1)])
;      keyword_set(med) : outarr[i] = median(arr[i*int:(i*int)+(int-1)])
;      keyword_set(max) : outarr[i] = max(arr[i*int:(i*int)+(int-1)],nan=nan)
;      keyword_set(min) : outarr[i] = min(arr[i*int:(i*int)+(int-1)],nan=nan)
      keyword_set(sd) : outarr[i] = stddev(arr[i*int:(i*int)+(int-1)],nan=nan,double=double)
      keyword_set(percentile) : BEGIN 
        parr = arr[i*int:(i*int)+(int-1)]
        gval = where(finite(parr),ngval)
        IF ngval GT 0 THEN outarr[i] = (parr[gval])[(sort(parr[gval]))[percentile*ngval/100.0]] ELSE outarr[i] = !value.f_nan
;        outarr[i] = (arr[i*int:(i*int)+(int-1)])[(sort(arr[i*int:(i*int)+(int-1)]))[percentile*n_elements(arr[i*int:(i*int)+(int-1)])/100.0]]
      END 
      ELSE : outarr[i] = mean(arr[i*int:(i*int)+(int-1)],nan=nan,double=double)
    ENDCASE 
  ENDFOR
  return,outarr
END

FUNCTION average_arr,arr,int,nan=nan,double=double,sz=sz,med=med,md=md,max=max,min=min,sd=sd,tot=tot,percentile=percentile

;more efficient function to average every int values
;padding is needed

  IF keyword_set(sd) OR keyword_set(md) OR (n_elements(percentile) EQ 1) THEN BEGIN
    return,average_arr_sdmd(arr,int,nan=nan,double=double,sz=sz,md=md,sd=sd,percentile=percentile)
  ENDIF ELSE BEGIN 
    sz = size(arr)
    n = n_elements(arr)
    ngrps = long(n / long(int))
    nout = ngrps * int
    narr = reform(arr[0:nout-1],int,ngrps)
    CASE 1 OF 
      keyword_set(med) : outarr = reform(median(narr,double=double,dim=1))
      keyword_set(max) : outarr = reform(max(narr,nan=nan,dim=1))
      keyword_set(min) : outarr = reform(min(narr,nan=nan,dim=1))  
      keyword_set(tot) : outarr = reform(total(narr,1,nan=nan))
      ELSE : outarr = reform(total(narr,1,nan=nan)/total(finite(narr),1))
    ENDCASE 
    return,outarr
  ENDELSE 
END


