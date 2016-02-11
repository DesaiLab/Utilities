PRO replacebad,arr,val
  i = where(isnan(arr),ni)
  IF ni GT 0 THEN arr[i] = val
END
