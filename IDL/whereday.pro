FUNCTION whereday,fjdays,yr=yr,lat=lat,lon=lon
;take an array of fjdays, and return 1 where daylight
;useful for making diurnal averages

  IF NOT keyword_set(yr) THEN yr = co2_const('cur_year')

  nelem = n_elements(fjdays)
  outarr = intarr(nelem)

  doys = fix(fjdays)
  tms = (fjdays - doys)*24.0
  uniqdoys = doys[uniq(doys,sort(doys))]

  rises = fltarr(n_elements(uniqdoys))
  sets = fltarr(n_elements(uniqdoys))

  FOR i = 0,n_elements(uniqdoys)-1 DO BEGIN
    dta = sunrise(uniqdoys[i],lat=lat,lon=lon,yr=yr,melarr=melarr)
    wheredoy = where((doys EQ uniqdoys[i]) AND (tms GE dta[0]) AND (tms LE dta[1]),ndoys)
    IF ndoys GT 0 THEN outarr[wheredoy] = 1
  ENDFOR

  return,outarr
END
