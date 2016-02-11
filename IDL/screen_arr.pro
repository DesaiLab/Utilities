FUNCTION screen_arr,arr,low,high,othervals,replace=replace
  ;replaces values in arr that are < low, > high, or othervals with replace
  IF NOT keyword_Set(replace) THEN replace = nan()
  IF isnan(replace) THEN bad = ((arr LT low) OR (arr GT high)) AND (isnotnan(arr)) ELSE bad = (arr LT low) OR (arr GT high)
  IF keyword_set(othervals) THEN bad = bad OR (1b-exclude(arr,othervals))
  locs = where(bad,num)
  IF num GT 0 THEN arr[locs] = replace
  return,arr
END
