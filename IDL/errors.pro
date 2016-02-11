FUNCTION Errors, x, rms=rms, abs=abs, relative=rel
  on_error, 2
  ;RMS = StdDev
  ;Absolute =   Sum(x-mean) / N
  ;Relative = Sum(x-mean) / mean * N
  if keyword_set(rms) then return, stddev(x)
  m = mean(x)
  t = total(x-m)
  if keyword_set(abs) then return, t / n_elements(x)
  if keyword_set(rel) then return, t / (n_elements(x) * m)
END
