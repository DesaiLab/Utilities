FUNCTION Normalize,x

;return function that has mean of 0 and variance of 1
  on_error, 2
  return, (x - mean(x,/nan)) / stddev(x,/nan)

END
