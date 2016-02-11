;Sunrise, Sunset, Solar noon calculator
;Source: http://www.srrb.noaa.gov/highlights/sunrise/sunrise.html
;Converted to IDL by Ankur Desai, 7 April 2003
;Main function is sunrise (at bottom)

FUNCTION DayofYear,mn,dy,yr,leapyear=leapyear
;Give month and day and year (can be arrays), return day of year
;year is optional, in which case keyword leapyear determines leapyearness

  IF NOT keyword_set(yr) THEN BEGIN
    IF keyword_set(leapyear) THEN lp = 1b ELSE lp = 0b
  ENDIF ELSE BEGIN 
    lp = ((((Yr mod 4) eq 0) and ((yr mod 100) ne 0)) or ((yr mod 400) eq 0))
  ENDELSE
  k = 2 - lp
  doy = floor((275*mn)/9) - k * floor((mn+9)/12) + dy - 30
  return,doy
END

FUNCTION CalcMDY,doy,yr,leapyear=leapyear
;Given the Day of Year and Year find the Month and Day of Month
;Year is optional

  IF NOT keyword_set(yr) THEN BEGIN
    IF keyword_set(leapyear) THEN lp = 1b ELSE lp = 0b
  ENDIF ELSE BEGIN 
    lp = ((((Yr mod 4) eq 0) and ((yr mod 100) ne 0)) or ((yr mod 400) eq 0))
  ENDELSE
  
  md = [31,28,31,30,31,30,31,31,30,31,30,31]
  IF lp THEN md[1] = 29
  modcum = fix(total(md,/cumulative))
  modcum = fix(doy)-(modcum - md)
  l = where(modcum le 0)
  if l[0] ne -1 then modcum[l] = 999
  dy = min(modcum)
  mo = (where(modcum eq dy))[0] + 1
  return,[mo,dy]
END 

FUNCTION calcJD,yr, mo, dy
;Calculate Julian Day (at 0 UTC) given Year, Month, Day
  year=long(yr)
  month = long(mo)
  IF month LE 2l THEN BEGIN
    year = year - 1l
    month = month + 12l
  ENDIF
  a = floor(year/100l)
  b = 2l - a + floor(a/4l)
  year = double(year)
  month = double(month)
  day = double(dy)
  JD = floor(365.25d*(year + 4716.0d)) + floor(30.6001d*(month+1.0d)) + day + B - 1524.5d
  return,jd
END

FUNCTION calcTimeJulianCent,jd
;convert Julian Day to centuries since J2000.0.	
  T = (double(jd) - 2451545.0d)/36525.0d
  return,t
END

FUNCTION calcJDFromJulianCent,t
;convert centuries since J2000.0 to Julian Day.	
  JD = double(t) * 36525.0d + 2451545.0d
  return,jd
END

FUNCTION calcGeomMeanLongSun,t
;calculate the Geometric Mean Longitude of the Sun (in degrees)
  L0 = (280.46646d + double(t) * (36000.76983d + 0.0003032d * double(t)))
  WHILE (l0 GT 360.0d) DO l0 = l0-360.0d
  WHILE (l0 LT 0.0d) DO l0 = l0+360.0d
  return,l0
END 

FUNCTION calcGeomMeanAnomalySun,t
;calculate the Geometric Mean Anomaly of the Sun (in degrees)
  M = 357.52911d + double(t) * (35999.05029d - 0.0001537d * double(t))
  return,m
END 

FUNCTION calcEccentricityEarthOrbit,t
;calculate the eccentricity of earth's orbit (unitless)
  e = 0.016708634d - double(t) * (0.000042037d + 0.0000001267d * double(t)) 
  return, e
END

FUNCTION calcSunEqOfCenter,t
;calculate the equation of center for the sun (in degrees)
  m = double(calcGeomMeanAnomalySun(t))
  mrad = (!dpi/180.0d)*m
  sinm = sin(mrad) 
  sin2m = sin(mrad+mrad) 
  sin3m = sin(mrad+mrad+mrad)
  C = sinm * (1.914602d - double(t) * (0.004817d + 0.000014d * double(t))) + sin2m * (0.019993d - 0.000101d * double(t)) + sin3m * 0.000289d 
  return,c
END

FUNCTION calcSunTrueLong,t
;calculate the true longitude of the sun (in degrees)
  l0 = calcGeomMeanLongSun(double(t))
  c = calcSunEqOfCenter(double(t))
  O = l0 + c               
  return,O
END

FUNCTION calcSunTrueAnomaly,t
;calculate the true anamoly of the sun (in degrees)
  m = calcGeomMeanAnomalySun(double(t))
  c = calcSunEqOfCenter(double(t)) 
  v = m + c
  return,v
END

FUNCTION calcSunRadVector,t
;calculate the distance to the sun in AU (in degrees)
  v = calcSunTrueAnomaly(double(t))
  e = calcEccentricityEarthOrbit(double(t)) 
  R = (1.000001018d * (1.0d - e * e)) / (1.0d + e * cos(((!dpi/180.0d)*v))) 
  return,r
END 

FUNCTION calcSunApparentLong,t
;calculate the apparent longitude of the sun (in degrees)
  o = calcSunTrueLong(double(t))
  omega = 125.04d - 1934.136d * double(t) 
  lambda = o - 0.00569d - 0.00478d * sin(((!dpi/180.0d)*omega)) 
  return,lambda
END 

FUNCTION calcMeanObliquityOfEcliptic,t
;calculate the mean obliquity of the ecliptic (in degrees)
  seconds = 21.448d - double(t)*(46.8150d + double(t)*(0.00059d - double(t)*(0.001813d))) 
  e0 = 23.0d + (26.0d + (seconds/60.0d))/60.0d 
  return,e0
END 

FUNCTION calcObliquityCorrection,t
;calculate the corrected obliquity of the ecliptic (in degrees)
  e0 = calcMeanObliquityOfEcliptic(double(t)) 
  omega = 125.04d - 1934.136d * double(t) 
  e = e0 + 0.00256d * cos(((!dpi/180.0d)*omega)) 
  return,e
END 

FUNCTION calcSunRtAscension,t
;calculate the right ascension of the sun (in degrees)
  e = calcObliquityCorrection(double(t)) 
  lambda = calcSunApparentLong(double(t)) 
  tananum = (cos(((!dpi/180.0d)*e)) * sin(((!dpi/180.0d)*lambda))) 
  tanadenom = (cos(((!dpi/180.0d)*lambda)))
  alpha = (180.0d/!dpi)*(atan(tananum, tanadenom))
  return,alpha
END 

FUNCTION calcSunDeclination,t
;calculate the declination of the sun (in degrees)
  e = calcObliquityCorrection(double(t)) 
  lambda = calcSunApparentLong(double(t)) 
  sint = sin(((!dpi/180.0d)*e)) * sin(((!dpi/180.0d)*lambda)) 
  theta = (180.0d/!dpi)*(asin(sint))
  return,theta
END 

FUNCTION calcEquationOfTime,t
;calculate the difference between true solar time and mean solar time
;(output: equation of time in minutes of time)	
  epsilon = calcObliquityCorrection(double(t)) 
  l0 = calcGeomMeanLongSun(double(t)) 
  e = calcEccentricityEarthOrbit(double(t)) 
  m = calcGeomMeanAnomalySun(double(t)) 
  y = tan(((!dpi/180.0d)*epsilon)/2.0d) 
  y = y * y
  sin2l0 = sin(2.0d * ((!dpi/180.0d)*l0)) 
  sinm   = sin(((!dpi/180.0d)*m)) 
  cos2l0 = cos(2.0d * ((!dpi/180.0d)*l0)) 
  sin4l0 = sin(4.0d * ((!dpi/180.0d)*l0))
  sin2m  = sin(2.0d * ((!dpi/180.0d)*m))
  Etime = y * sin2l0 - 2.0d * e * sinm + 4.0d * e * y * sinm * cos2l0 - 0.5d * y * y * sin4l0 - 1.25d * e * e * sin2m
  return,(180.0d/!dpi)*Etime*4.0d
END 

FUNCTION calcHourAngleSunrise,lat,solarDec,civil=civil,nautical=nautical,astronomical=astronomical
;calculate the hour angle of the sun at sunrise for the latitude (in radians)
  angle = 90.0d + (5.0d/6.0d)
  IF keyword_set(civil) THEN angle = 96.0d
  IF keyword_set(nautical) THEN angle = 102.0d
  IF keyword_set(astronomical) THEN angle = 108.0d

  latRad = ((!dpi/180.0d)*double(lat))   
  sdRad  = ((!dpi/180.0d)*double(solarDec))
  HAarg = (cos(((!dpi/180.0d)*angle))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad)) 
  HA = (acos(cos(((!dpi/180.0d)*angle))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad))) 
  return,ha
END 

FUNCTION calcHourAngleSunset,lat, solarDec,civil=civil,nautical=nautical,astronomical=astronomical
;calculate the hour angle of the sun at sunset for the latitude (in radians)
  angle = 90.0d + (5.0d/6.0d)
  IF keyword_set(civil) THEN angle = 96.0d
  IF keyword_set(nautical) THEN angle = 102.0d
  IF keyword_set(astronomical) THEN angle = 108.0d

  latRad = ((!dpi/180.0d)*double(lat))   
  sdRad  = ((!dpi/180.0d)*double(solarDec))
  HAarg = (cos(((!dpi/180.0d)*angle))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad))
  HA = (acos(cos(((!dpi/180.0d)*angle))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad)))
  return,(-1.0d)*ha
END   

FUNCTION calcSunriseUTC,JD, latitude, longitude,civil=civil,nautical=nautical,astronomical=astronomical
;calculate time of sunrise for the given day at the given location on earth
;(in minute since 0 UTC)
  t = calcTimeJulianCent(double(jd))
  eqTime = calcEquationOfTime(double(t)) 
  solarDec = calcSunDeclination(double(t)) 
  hourAngle = calcHourAngleSunrise(double(latitude), double(solarDec),civil=civil,nautical=nautical,astronomical=astronomical) 
  delta = double(longitude) - ((180.0d/!dpi)*hourAngle) 
  timeDiff = 4.0d * delta    
  timeUTC = 720.0d + timeDiff - eqTime
  newt = calcTimeJulianCent(calcJDFromJulianCent(double(t)) + timeUTC/1440.0d) 
  eqTime = calcEquationOfTime(double(newt)) 
  solarDec = calcSunDeclination(double(newt)) 
  hourAngle = calcHourAngleSunrise(double(latitude), double(solarDec),civil=civil,nautical=nautical,astronomical=astronomical) 
  delta = double(longitude) - ((180.0d/!dpi)*hourAngle)
  timeDiff = 4.0d * delta          
  timeUTC = 720.0d + timeDiff - eqTime 
  return,timeutc
END 

FUNCTION calcSunsetUTC,JD, latitude, longitude,civil=civil,nautical=nautical,astronomical=astronomical
;calculate time of sunset for the given day at the given location on earth
;(in minute since 0 UTC)
  t = calcTimeJulianCent(double(JD))
  eqTime = calcEquationOfTime(double(t)) 
  solarDec = calcSunDeclination(double(t)) 
  hourAngle = calcHourAngleSunset(double(latitude), double(solarDec),civil=civil,nautical=nautical,astronomical=astronomical) 
  delta = double(longitude) - ((180.0d/!dpi)*hourAngle) 
  timeDiff = 4.0d * delta     
  timeUTC = 720.0d + timeDiff - eqTime 
  newt = calcTimeJulianCent(calcJDFromJulianCent(double(t)) + timeUTC/1440.0d) 
  eqTime = calcEquationOfTime(double(newt)) 
  solarDec = calcSunDeclination(double(newt)) 
  hourAngle = calcHourAngleSunset(double(latitude), double(solarDec),civil=civil,nautical=nautical,astronomical=astronomical)
  delta = double(longitude) - ((180.0d/!dpi)*hourAngle) 
  timeDiff = 4.0d * delta         
  timeUTC = 720.0d + timeDiff - eqTime 
  return,timeutc

END   

FUNCTION calcSolNoonUTC,jd, longitude
;calculate time of solar noon the given day at the given location on earth
;(in minute since 0 UTC)
  t = calcTimeJulianCent(double(JD))
  newt = calcTimeJulianCent(calcJDFromJulianCent(double(t)) + 0.5d + double(longitude)/360.0d) 
  eqTime = calcEquationOfTime(double(newt)) 
  solarNoonDec = calcSunDeclination(double(newt)) 
  solNoonUTC = 720.0d + (double(longitude) * 4.0d) - eqTime 
  return,solnoonutc
END 

FUNCTION sunrise,doy,yr,lat=lat,lon=lon,melarr=melarr,civil=civil,nautical=nautical,astronomical=astronomical
;Main sunrise/sunset calculator by Ankur Desai April 2003

;DOY is Day of Year (1 = 1/1/2003 to 365 or 366)
;  Default is Today (from computer clock)
;Yr is the Year - default is this year

;DOY and/or YR can be arrays (one or both)

;Lat and Lon are latitude and longitude (default is Sylvania Flux Tower)
;melarr is just to keep some comptibility with old code
;Do not give a latitude above the (ant)arctic circle in mid winter or summer
;Negative Longitide is west
;Positive latitude is north

;Default time is Official sunset and sunrise (zenith angle of 90.833)
;Set keyword Civil for civil sunset/rise (96 degrees)
;            Nautical is 102 degrees
;            Astronomical is 108 degrees

;Output
;returns time (in hours UTC) of [sunrise,sunset,local noon,length of day]
;If DOY and/or YR is an array, then you get a bunch of these in rows

;Example:
;print,sunrise(180,2002,lat=40.0,lon=-90.0)
;   10.564731   25.548302   18.057778   14.983571
; sunrise (UTC)   sunset      noon      day length (hrs)  

  caldat,systime(/julian),mo,day,year
  IF NOT keyword_Set(doy) THEN BEGIN 
    yr = year
    doy = DayofYear(mo,day,yr)
  ENDIF ELSE IF NOT keyword_set(yr) THEN yr = year
    
  IF NOT keyword_set(lat) THEN lat = 46.242017d
  IF NOT keyword_set(lon) THEN lon = -89.347650d

;calculate month and day from Day of Year and then find Julian Day

  nelem = max([n_elements(doy),n_elements(yr)])

  output = dblarr(4,nelem)
  FOR i = 0,nelem-1 DO BEGIN 
    IF n_elements(doy) GT 1 THEN thedoy = doy[i] ELSE thedoy = doy
    IF n_elements(yr) GT 1 THEN theyr = yr[i] ELSE theyr = yr

    md = CalcMDY(thedoy,theyr)
    mo = md[0]
    dy = md[1]    
    jd = calcjd(theyr,mo,dy)

    rise = CalcSunriseUTC(jd,lat,lon*(-1.0d),civil=civil,nautical=nautical,astronomical=astronomical)/60.0d
    set = CalcSunSetUTC(jd,lat,lon*(-1.0d),civil=civil,nautical=nautical,astronomical=astronomical)/60.0d
    noon = CalcSolNoonUTC(jd,lon*(-1.0d))/60.0d
    daylen = set-rise
    output[*,i] = [rise,set,noon,daylen]
  ENDFOR

  return,reform(output)
END

