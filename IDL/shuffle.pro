FUNCTION shuffle,nvals,seed=seed
;produce nvals unique random numbers range 0 to nvals-1
;using Fisher-Yates Shuffle 
  n = nvals
  IF n_elements(seed) EQ 0 THEN seed = systime(/sec)
  locs = lindgen(n)
  WHILE (n GT 1) DO BEGIN
    k = long(randomu(seed)*n)
    n--
    temp = locs[n]
    locs[n] = locs[k]
    locs[k] = temp
  ENDWHILE
  return,locs
END
