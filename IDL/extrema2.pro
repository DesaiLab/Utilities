;faster extreme using uniq and shift

FUNCTION extrema2,data,maxima=maxima,minima=minima

  nx = n_elements(data)

  pdata = [data[0],data,data[nx-1]]
  
  n = uniq(data)

  IF n_elements(n) EQ 1 THEN BEGIN
    maxima = 0
    minima = 0
    return,0
  ENDIF ELSE BEGIN 
    m = [-1,n-n[1:*]]+1
    nn = n + m

    nnn=nn+1
    
    spd = shift(pdata,1)
    opd = shift(pdata,-1)
    
    mx = where(pdata[nnn] GT spd[nnn] AND pdata[nnn] GT opd[nnn],nmx)
    mn = where(pdata[nnn] LT spd[nnn] AND pdata[nnn] LT opd[nnn],nmn)
    
    IF nmx GT 0 THEN maxima = nn[mx]
    IF nmn GT 0 THEN minima = nn[mn]
    
    oval = [0]
    IF nmx GT 0 THEN oval = [oval,maxima]
    IF nmn GT 0 THEN oval = [oval,minima]
    
    IF n_elements(oval) GT 1 THEN oval = oval[1:*]
;  stop
    
    return,oval[sort(oval)]
  ENDELSE 

END
