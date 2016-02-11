FUNCTION readnarr,yr,mo,day,var,times=times,levels=levels,lats=lats,lons=lons,fixed=fixed,opendap=opendap
;READ subsets of data directly for the NOAA North American Regional
;Renalaysis using the OpenDAP protocol

;This version is designed for use with the "loaddods.pro" reader, the
;DODS application package (writeval) - use on flux
  
;Can also use the official OpenDAP interface if it is installed and on
;the path (opendap_get) - currently not installed on flux (requires
;                         OCAPI and other packages)

;Inputs
;Yr = Year (1979 onward)
;Mo = Month (1-12)
;Day = Day (1-31)
;Var can be a string with the varname from the NARR files or an array
;of strings for mutiple variables

;Optional inputs
;Times - in UTC hours from 0-24, can be a single value, or a range
;        vector (min to max)
;Levels - Range from 1-29, if not included, the variable is assumed to
;         not have levels. MUST include if variables has vertical levels
;Lats - in degrees N (single value or [min,max])
;Lons - in degrees E (single value or [min,max])
;Fixed - Read "fixed" values from the AWIP32 fixed file - year/mo/day
;        are disregarded
;opendap - Use the official opendap interface

;Outputs
;Returns a structure which contains a structure for each variable
;e.g. f = readnarr(2007,10,5,['hpbl','snowc'],lats=[40,45],lons=[-110,-100])
; f.hpbl and f.snowc will be output - each is a structure
;f.hpbl.data (the actual PBL height data), f.hpbl.lat f.hpbl.lon (lat and lons)


  timearr = findgen(8)*3
  lonarr = numgen(-220.0,-0.625,numval=586)
  latarr = findgen(240)*(90./240.)

  lonarr = rebin(lonarr,1172)
  latarr = rebin(latarr,480)


  IF keyword_seT(fixed) THEN BEGIN
    lonarr = rebin(lonarr,1172)
    latarr = rebin(latarr,480)
    IF n_elements(lats) EQ 0 THEN glats = [0,479] ELSE BEGIN
      glats=lats
      FOR i = 0,n_elements(lats)-1 DO glats[i]=closest(latarr,lats[i])
    ENDELSE 
    IF n_elements(lons) EQ 0 THEN glons = [0,1171] ELSE BEGIN
      glons=lons
      FOR i = 0,n_elements(lons)-1 DO glons[i]=closest(lonarr,lons[i])
    ENDELSE 
    gtimes = [0]
  ENDIF ELSE BEGIN 
    IF n_elements(lats) EQ 0 THEN glats = [0,239] ELSE BEGIN
      glats=lats
      FOR i = 0,n_elements(lats)-1 DO glats[i]=closest(latarr,lats[i])
    ENDELSE 
    IF n_elements(times) EQ 0 THEN gtimes = [0,7] ELSE BEGIN
      gtimes=times
      FOR i = 0,n_elements(times)-1 DO gtimes[i]=closest(timearr,times[i])
    ENDELSE 
    IF n_elements(lons) EQ 0 THEN glons = [0,585] ELSE BEGIN
      glons=lons
      FOR i = 0,n_elements(lons)-1 DO glons[i]=closest(lonarr,lons[i])
    ENDELSE 
  ENDELSE 

;  IF n_elements(levels) EQ 0 THEN levels = [0,28] ;fix
  IF n_elements(levels) NE 0 THEN levels<=28
  
                                ;convert times in UTC, levels in Pa or
                                ;m, lats in degrees, lons in degrees
                                ;to model levels


  IF n_elements(glats) EQ 1 THEN latstr = '['+string(glats[0],format='(i0)')+']' ELSE latstr = '['+string(glats[0],format='(i0)')+':'+string(glats[1],format='(i0)')+']'
  IF n_elements(gtimes) EQ 1 THEN tmstr = '['+string(gtimes[0],format='(i0)')+']' ELSE tmstr = '['+string(gtimes[0],format='(i0)')+':'+string(gtimes[1],format='(i0)')+']'
  IF n_elements(glons) EQ 1 THEN lonstr = '['+string(glons[0],format='(i0)')+']' ELSE lonstr = '['+string(glons[0],format='(i0)')+':'+string(glons[1],format='(i0)')+']'  

  IF n_elements(levels) NE 0 THEN BEGIN 
    IF n_elements(levels) EQ 1 THEN levstr = '['+string(levels[0],format='(i0)')+']' ELSE levstr =  '['+string(levels[0],format='(i0)')+':'+string(levels[1],format='(i0)')+']' 
  ENDIF ELSE levstr = ''

;some variables are in b file
;get list to set

;for time series, can use subset files (Esp surfmet - see what it has)  

  IF keyword_set(fixed) THEN BEGIN
    fname = 'http://205.167.25.166:80/dods/NCEP_NARR_DAILY/AWIP32-fixed'
    f = loaddods(fname,var,'[0]'+latstr+lonstr,opendap=opendap)
  ENDIF ELSE BEGIN 
    YYYY = string(yr,format='(i4.4)')
    MM = string(mo,format='(i2.2)')
    DD = string(day,format='(i2.2)')
    if (yr eq 2010 and mo eq 9 and day ge 10) or $
       (yr eq 2010 and mo gt 9) or $
       (yr eq 2011 and mo lt 3) or $
       (yr eq 2011 and mo le 3 and day le 10) $ 
    then prefixn = 'hh' else prefixn = '00'
    fname = 'http://205.167.25.166:80/dods/NCEP_NARR_DAILY/'+YYYY+MM+'/'+YYYY+MM+DD+'/narr-a_221_'+YYYY+MM+DD+'_'+prefixn+'00_000'
    f = loaddods(fname,var,tmstr+levstr+latstr+lonstr,opendap=opendap)
    IF f.error EQ 1 THEN BEGIN
      IF prefixn EQ 'hh' THEN prefixn = '00' ELSE prefixn = 'hh'
      fname = 'http://205.167.25.166:80/dods/NCEP_NARR_DAILY/'+YYYY+MM+'/'+YYYY+MM+DD+'/narr-a_221_'+YYYY+MM+DD+'_'+prefixn+'00_000'
      f = loaddods(fname,var,tmstr+levstr+latstr+lonstr,opendap=opendap)
    ENDIF 
  ENDELSE 

; IDL> url = 'http://test.opendap.org/opendap-3.4/nph-dods/data/nc/fnoc1.nc'
; IDL> stat = OPENDAP_GET(url, data)  
  return,f

END

