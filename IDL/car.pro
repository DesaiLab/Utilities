FUNCTION Car,a,n
  IF NOT keyword_set(n) THEN n = 1
  return,a[0:n-1]
END
