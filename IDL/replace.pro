FUNCTION replace,arr,old,new,loc=loc,nloc=nloc
  IF finite(old) THEN loc = where(arr EQ old,nloc) ELSE loc = where(~finite(arr),nloc)
  IF nloc GT 0 THEN arr[loc] = new
  return,arr
END
