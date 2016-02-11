FUNCTION readnldas,yr,startday=startday,endday=endday,var=var,lats=lats,lons=lons
;open das URL is:
;http://hydro1.sci.gsfc.nasa.gov/dods/NLDAS_FORA0125_H.002
;http://169.154.132.77/dods/NLDAS_FORA0125_H.002

;http://169.154.132.77/dods/NLDAS_FORA0125_H.002?var[time][lat][lon]


;time = 312768 - 13Z 1 Jan 1979 to 12Z 6 Sep
;long is 464 from -124.9375 to -67.0625 (delta is 0.125) - vals are
;                                                          centered,
;start at -125
;lat is 224 from 25.0625 to 52.9375  (delta is 0.125) - vals are
;                                                       centered -
;start at 25

;time is days since 1-1-1 00:00:0.0

;julday(9,6,2014,12)-julday(1,1,1,0)

;vars
 
;; apcpsfc ** precipitation hourly total [kg/m^2] 
;; cape180_0mb ** 180-0 mb above ground convective available potential energy [j/kg] 
;; convfracsfc ** fraction of total precipitation that is convective [unitless] 
;; dlwrfsfc ** lw radiation flux downwards (surface) [w/m^2] 
;; dswrfsfc ** sw radiation flux downwards (surface) [w/m^2] 
;; pevapsfc ** potential evaporation [kg/m^2] 
;; pressfc ** surface pressure [pa] 
;; spfh2m ** 2-m above ground specific humidity [kg/kg] 
;; tmp2m ** 2-m above ground temperature [k] 
;; ugrd10m ** 10-m above ground zonal wind speed [m/s] 
;; vgrd10m ** 10-m above ground meridional wind speed [m/s] 

  IF n_elements(var) EQ 0 THEN var = ['tmp2m','ugrd10m','vgrd10m','spfh2m','pressfc','dlwrfsfc','dswrfsfc','apcpsfc']

  lonarr = numgen(-124.9375,-0.125,numval=464)
  latarr = numgen(25.0625,52.9375,0.125,numval=224)

  IF n_Elements(yr) EQ 0 THEN yr = 2013
  ystr = string(yr,format='(i4.4)')
  
;time starts at 722451.54 

  IF n_elements(endday) EQ 0 AND n_elements(startday) NE 0 THEN endday=startday
  IF n_Elements(startday) EQ 0 THEN startday = 1
  IF n_elements(endday) EQ 0 THEN endday = days_in_year(yr)

  sstr = jd_to_dy(startday,y=yr)
  estr = jd_to_dy(endday,y=yr)

  tloc1 = (julday(long(strmid(sstr,4,2)),long(strmid(sstr,6,2)),yr,0)-julday(1,1,1979,13))*24
  tloc2 = (julday(long(strmid(estr,4,2)),long(strmid(estr,6,2)),yr,23)-julday(1,1,1979,13))*24

  gtimes = [tloc1,tloc2]

  IF n_elements(lats) EQ 0 THEN glats = [167] ELSE BEGIN
    glats=lats
    FOR i = 0,n_elements(lats)-1 DO glats[i]=closest(latarr,lats[i])
  ENDELSE 
  IF n_elements(lons) EQ 0 THEN glons = [129] ELSE BEGIN
    glons=lons
    FOR i = 0,n_elements(lons)-1 DO glons[i]=closest(lonarr,lons[i])
  ENDELSE 

  IF n_elements(glats) EQ 1 THEN latstr = '['+string(glats[0],format='(i0)')+']' ELSE latstr = '['+string(glats[0],format='(i0)')+':'+string(glats[1],format='(i0)')+']'
  IF n_elements(gtimes) EQ 1 THEN tmstr = '['+string(gtimes[0],format='(i0)')+']' ELSE tmstr = '['+string(gtimes[0],format='(i0)')+':'+string(gtimes[1],format='(i0)')+']'
  IF n_elements(glons) EQ 1 THEN lonstr = '['+string(glons[0],format='(i0)')+']' ELSE lonstr = '['+string(glons[0],format='(i0)')+':'+string(glons[1],format='(i0)')+']'  

  fname = 'http://169.154.132.77/dods/NLDAS_FORA0125_H.002'

;stop
  f = loaddods(fname,var,tmstr+latstr+lonstr,opendap=opendap)

  return,f

END
