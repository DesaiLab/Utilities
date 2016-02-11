FUNCTION average_cols,arr,int,colarr,nan=nan,double=double,med=med,md=md,max=max,min=min,sd=sd,tot=tot
  IF keyword_set(double) THEN type = 5 ELSE type = 4
  sz = size(arr)
  IF NOT keyword_set(colarr) THEN colarr = lindgen(sz[1])
  nelem = sz[2]
  newnelem = long(nelem/long(int))
  ncols = n_elements(colarr)
  outarr = make_array(ncols,newnelem,type=type)
  FOR k = 0l,ncols-1l DO outarr[k,*] = average_arr(arr[colarr[k],*],int,nan=nan,double=double,sz=sz,med=med,md=md,max=max,min=min,sd=sd,tot=tot)
  return,outarr
END
