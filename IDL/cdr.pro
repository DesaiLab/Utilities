FUNCTION Cdr,a,n
  IF NOT keyword_set(n) THEN n = 1
  return,a[n:n_elements(a)-1]
END
