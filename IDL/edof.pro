FUNCTION edof,x,y
;effective degrees of freedom of one dataset or two (for significance
;testing)
;useful for temporally autocorrelated time series
  IF n_elements(y) EQ 0 THEN y=x
  n = float(min([n_elements(x),n_elements(y)]))
  lag = findgen(n)-(n/2)
  rho_x = a_correlate(zapbadval(x),lag)
  rho_y = a_correlate(zapbadval(y),lag)
  neff = n / total((1.0 - (lag/n))*rho_x*rho_y,/nan)
  return,neff

END
