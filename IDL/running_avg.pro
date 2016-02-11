FUNCTION running_avg,in,w
;running average of vector in using w averaging points (must be odd)
;in = input data vector, w = # points to average over (must be odd)
;returns, averaged vector
  narr = replicate(1.0,w)
  return,convol(in,narr,w,/center)
END
