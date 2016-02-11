FUNCTION scalar_angle,theta
;compute scalar mean from set of angles
;from http://www.webmet.com/met_monitoring/621.html
  gval=where(finite(theta),ngval)
  IF ngval GT 0 THEN BEGIN 
    Di = fltarr(ngval)
    FOR i = 0l,ngval-1l DO BEGIN
      IF i EQ 0 THEN Di[i] = theta[gval[i]] ELSE BEGIN
        delta = theta[gval[i]] - Di[i-1]
        IF delta LT -180 THEN Di[i]=Di[i-1]+delta+360
        IF abs(delta) LT 180 THEN Di[i]=Di[i-1]+delta
        IF delta GT 180 THEN Di[i]=Di[i-1]+delta-360
      ENDELSE  
    ENDFOR
    Dd=total(Di)/ngval
    WHILE (Dd LT 0) DO Dd+=360
    WHILE (Dd GE 360) DO Dd-=360
  ENDIF ELSE Dd = !values.f_nan
  return,Dd
END

