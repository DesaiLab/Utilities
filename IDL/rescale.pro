FUNCTION rescale,arr,oldrange,newrange,type,double=double

  IF keyword_set(double) THEN rangetype = 'double' ELSE rangetype = 'float'
  newor = call_FUNCTION(rangetype,oldrange)
  newnr = call_FUNCTION(rangetype,newrange)
  lineslope,[newor[0],newnr[0],newor[1],newnr[1]],m,b
  newarr = b + (m * arr)
  IF keyword_set(type) THEN newarr = call_FUNCTION(type,newarr)
  return,newarr
END
