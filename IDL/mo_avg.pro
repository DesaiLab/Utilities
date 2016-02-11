FUNCTION mo_avg,data,doy,yr=yr,nan=nan,sd=sd
;compute monthly averages
  IF n_elements(yr) EQ 0 THEN yr = 1999
  IF n_elements(doy) EQ 0 THEN doy = (findgen(n_elements(data)) MOD dayS_in_year(yr))+1
  mo = float(mo_doy(doy,y=yr))
  gmo = uniq(mo)
  IF gmo[n_elements(gmo)-1] NE n_elements(mo)-1 THEN gmo=[gmo,n_elements(gmo)-1]
  gmos = [0,gmo[0:n_elements(gmo)-2]+1]
  newdata = fltarr(n_elements(gmos))
  IF keyword_set(sd) THEN BEGIN
    FOR i = 0,n_elementS(gmos)-1 DO newdata[i] = stddev(data[gmos[i]:gmo[i]],nan=nan)
  ENDIF ELSE BEGIN 
    FOR i = 0,n_elementS(gmos)-1 DO newdata[i] = mean(data[gmos[i]:gmo[i]],nan=nan)
  ENDELSE 
;  stop

;  newdata = bin_Avg(mo,data,min=1,max=12,nbins=12,locations=l)
  return,newdata
END
