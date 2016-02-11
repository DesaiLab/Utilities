FUNCTION isallnan,vals
  i = where(isnan(vals),numi)
  n = n_elements(vals)
  return,numi EQ n
END
