FUNCTION esat,temp
;temp in C, compute esat in mb
  Es = 6.112 * exp( (17.67 * temp) / (243.5 + temp))
  return,es
END
