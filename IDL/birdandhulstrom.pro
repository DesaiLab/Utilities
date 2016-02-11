PRO calcSolarposition,jd, latitude, longitude, solarazimuth=solarazimuth, solarelevation=solarelevation, solarzenith=solarzenith
;calculate solar position for the entered date, time and location. 

  t = calcTimeJulianCent(double(JD))
  R = calcSunRadVector(double(t))
  alpha = calcSunRtAscension(double(t))
  theta = calcSunDeclination(double(t))
  Etime = calcEquationOfTime(double(t))
  eqtime = Etime
  SolarDec = theta              ;//    in degrees
  earthRadVec = R
  zone = 0.0
  solarTimeFix = eqtime - 4.0d * longitude + 60.0d * zone

  caldat,double(jd),month,day,year,hh,mm,ss

  trueSolarTime = hh * 60.0d + mm + SS / 60.0d + solarTimeFix
  WHILE (trueSolarTime GT 1440) DO trueSolarTime = trueSolarTime - 1440
  hourangle = trueSolarTime / 4.0d - 180.0d
  IF (hourangle LT (-180.0d)) THEN hourangle = hourangle + 360.0
  harad = ((!dpi/180.0d)*hourangle)
  csz = Sin(((!dpi/180.0d)*Latitude)) * Sin(((!dpi/180.0d)*SolarDec)) + Cos(((!dpi/180.0d)*Latitude)) * Cos(((!dpi/180.0d)*SolarDec)) * Cos(harad)

  IF (csz GT 1.0d) THEN csz = 1.0 ELSE IF (csz LT (-1.0d)) THEN csz = -1.0d
            
  zenith = ((180.0d/!dpi)*Acos(csz))
  
  azDenom = (Cos(((!dpi/180.0d)*Latitude)) * Sin(((!dpi/180.0d)*zenith)))
            
  If (Abs(azDenom) GT 0.001) THEN BEGIN 
    azRad = ((Sin(((!dpi/180.0d)*Latitude)) * Cos(((!dpi/180.0d)*zenith))) - Sin(((!dpi/180.0d)*SolarDec))) / azDenom
    If (Abs(azRad) GT 1.0d) THEN IF (azRad LT 0.0d) THEN azRad = (-1.0d) ELSE azRad = 1.0d
    azimuth = 180.0d - ((180.0d/!dpi)*Acos(azRad))
    If (hourangle GT 0.0d) THEN azimuth = (-1.0d) * azimuth
  ENDIF ELSE IF (Latitude GT 0.0d) THEN azimuth = 180.0d ELSE azimuth = 0.0d
  If (azimuth LT 0.0d) THEN azimuth = azimuth + 360.0d
  exoatmElevation = 90.0d - zenith

  If (exoatmElevation GT 85.0d) THEN refractionCorrection = 0.0 ELSE BEGIN 
    te = Tan(((!dpi/180.0d)*exoatmElevation))
    IF (exoatmElevation GT 5.0) THEN BEGIN 
      refractionCorrection = 58.1 / te - 0.07 / (te * te * te) + 0.000086 / (te * te * te * te * te)
    ENDIF ELSE BEGIN 
      IF (exoatmElevation GT (-0.575d)) THEN BEGIN 
        step1 = (-12.79d + exoatmElevation * 0.711d)
        step2 = (103.4d + exoatmElevation * (step1))
        step3 = (-518.2d + exoatmElevation * (step2))
        refractionCorrection = 1735.0d + exoatmElevation * (step3)
      ENDIF ELSE BEGIN 
        refractionCorrection = -20.774 / te
      ENDELSE
    ENDELSE 
    refractionCorrection = refractionCorrection / 3600.0d
  ENDELSE 
            
  solarzenith = zenith - refractionCorrection
  solarazimuth = azimuth
  solarelevation = 90.0d - solarzenith

END 

PRO BirdAndHulstrom,doy, hh, mm, ss, yr=yr, pressureMb=pressureMb, $
                    ozoneCm=ozoneCm, waterCm=WaterCm, AOD500nm=AOD500nm, $
                    AOD380nm=AOD380nm, Ba=Ba, albedo=albedo, $
                    directBeam=directBeam, directHz=directHz, globalHz=globalHz, $
                    diffuseHz=diffuseHz, birdETR=birdETR, lat=lat, lon=lon, $
                    solarazimuth=solarazimuth, solarelevation=solarelevation, $
                    solarzenith=solarzenith
  
;Bird and Hulstrom's solar radiation model
;From the publication "A Simplified Clear Sky model
;for Direct and Diffuse Insolation on Horizontal Surfaces"
;by R.E. Bird and R.L Hulstrom, SERI Technical Report
;SERI/TR-642-761, Feb 1991.
;Solar Energy Research Institute, Golden, CO.

;Inputs:
;DOY = Day of Year - optional (default: use current day and time)
;HH = Hour (UTC) - optional (default is 0)
;MM = Minutes - optional (default is 0)
;SS = Seconds - optional (default 0)
;Yr = Year - optional (use current year)
;pressureMb = barometric pressure (default is 947 mb)
;ozoneCm = ozone thickness of atm (default is 0.3 cm)
;waterCm = water vapor thickness of atm (default is 1.5 cm)
;AOD500nm = aerosol optical depth at 500 nm (default is 0.1)
;AOD380nm = aerosol optical depth at 380 nm (default is 0.05)
;Ba = forward scattering of incoming radiation (default is 0.85)
;albedo = surface albedo (default is 0.25)
;lat = latitude (default is Sylvania flux tower)
;lon = longitude (default is Sylvania flux tower)

;Outputs:
;BirdETR = Extraterrestrial radiation normal to beam
;DirectBeam = radiation incident at Earth surface normal to beam
;directHz = direct radiation incident on a horizontal surface
;diffuseHz = diffuse radiation incident on a horizontal surface
;globalHz = global radiation incident on a horizontal surface
;solarazimuth, solarzenith, solarelevation - solar position values (in degrees)

  comsun = sunrise() ;force compilation of sunrise routines
  caldat,systime(/julian),mo,day,year,hour,minute,second
  IF NOT keyword_set(yr) THEN yr = year
  IF NOT keyword_set(doy) THEN BEGIN
    doy = DayofYear(mo,day,yr) & hh = hour & mm = minute & ss = second
  ENDIF ELSE BEGIN
    IF NOT keyword_set(hh) THEN BEGIN
      hh = 0.0 
      IF NOT keyword_set(mm) THEN mm = 0.0 
      IF NOT keyword_set(ss) THEN ss = 0.0
    ENDIF ELSE BEGIN
      IF NOT keyword_set(mm) THEN BEGIN
        mm = 0.0 
        IF NOT keyword_set(ss) THEN ss = 0.0
      ENDIF ELSE BEGIN
        IF NOT keyword_set(Ss) THEN BEGIN
          ss = 0.0
        ENDIF
      ENDELSE 
    ENDELSE  
  ENDELSE  

  md = CalcMDY(doy,yr)
  jd = julday(md[0],md[1],yr,hh,mm,ss)

  IF NOT keyword_set(lat) THEN lat = 46.242017d
  IF NOT keyword_set(lon) THEN lon = -89.347650d

  calcSolarposition,jd, lat, lon*(-1.0), solarazimuth=solarazimuth, solarelevation=solarelevation, solarzenith=solarzenith
  zen = (!dpi/180.0d)*solarzenith
  
  IF NOT keyword_set(pressureMb) THEN pressureMb = 947.0 ;sea level = 1013
  IF NOT keyword_set(ozoneCm) THEN ozoneCm = 0.3 ;typical 0.05-0.4
  IF NOT keyword_set(waterCm) THEN waterCm = 1.5 ;typical 0.01-6.5
  IF NOT keyword_set(AOD500nm) THEN AOD500nm = 0.1 ;typical 0.02-0.5
  IF NOT keyword_Set(AOD380nm) THEN AOD380nm = 0.05 ;typical 0.1-0.5
  IF NOT keyword_set(Ba) THEN Ba = 0.85 ;typical 0.85
  IF NOT keyword_set(albedo) THEN albedo = 0.25 ;0.2 land, 0.25 veg, 0.9 snow

;First, calculate extraterrestrial radiation (birdETR) corrected for earth radius vector
;based on Partridge, G. W. and Platt, C. M. R. 1976.
;Radiative Processes in Meteorology and Climatology.
;as described by
;http://solardat.uoregon.edu/SolarRadiationBasics.html

  lp = ((((Yr mod 4) eq 0) and ((yr mod 100) ne 0)) or ((yr mod 400) eq 0))
  IF lp THEN days_in_year = 366 ELSE days_in_year = 365
  RavBeta = 2.0d * !dpi * doy / days_in_year

  Rav2Rsq = 1.00011d + 0.034221d * Cos(RavBeta) + 0.00128d * Sin(RavBeta) + 0.000719d * Cos(2.0d * RavBeta) + 0.000077d * Sin(2.0d * RavBeta)
  solConNREL = 1367.0d             ;NREL uses solar constant of 1367 W/m^2
  birdETR = solConNREL * Rav2Rsq

;Next, calculate insolation using Bird and Hulstrom model
  tauA = 0.2758 * AOD380nm + 0.35 * AOD500nm 
;broadband aerosol optical depth - typical values range from 0.02 to 0.5

  If ((180.0d/!dpi)*ZEN) LT 89 THEN BEGIN 
    airMass = 1 / (Cos(ZEN) + 0.15 / (93.885 - ((180.0d/!dpi)*ZEN)) ^ 1.25)
    tRayliegh = Exp(-0.0903 * (pressureMb / 1013) ^ 0.84 * (1 + pressureMb / 1013 - (pressureMb / 1013) ^ 1.01))
    tOzone = 1 - 0.1611 * (ozoneCm * airMass) * (1 + 139.48 * (ozoneCm * airMass)) ^ (-0.3034) - 0.002715 * (ozoneCm * airMass) / (1 + 0.044 * (ozoneCm * airMass) + 0.0003 * (ozoneCm * airMass) ^ 2)
    tGases = Exp(-0.0127 * airMass * pressureMb / 1013)
    tWater = 1 - 2.4959 * airMass * waterCm / ((1 + 79.034 * waterCm * airMass) ^ 0.6828 + 6.385 * waterCm * airMass)
    tAerosol = Exp(-(tauA ^ 0.873) * (1 + tauA - tauA ^ 0.7088) * airMass ^ 0.918)
    TAA = 1 - 0.1 * (1 - airMass + airMass ^ 1.06) * (1 - tAerosol)
    rs = 0.0685 + (1 - Ba) * (1 - tAerosol / TAA)
    Id = 0.9662 * birdETR * tAerosol * tWater * tGases * tOzone * tRayliegh
    Ias = birdETR * Cos(ZEN) * 0.79 * tOzone * tGases * tWater * TAA * (0.5 * (1 - tRayliegh) + Ba * (1 - (tAerosol / TAA)) / (1 - airMass + (airMass) ^ 1.02))
  ENDIF ELSE BEGIN 
    airMass = 0.0d
    tRayliegh = 0.0d
    tOzone = 0.0d
    tGases = 0.0d
    tWater = 0.0d
    tAerosol = 0.0d
    TAA = 0.0d
    rs = 0.0d
    Id = 0.0d
    Ias = 0.0d
  ENDELSE 

  IF ((180.0d/!dpi)*ZEN) LT 90 THEN IdnH = Id * Cos(ZEN) ELSE IdnH = 0
  If airMass GT 0 THEN GH = (IdnH + Ias) / (1 - albedo * rs) ELSE GH = 0
  directBeam = Id
  directHz = IdnH
  globalHz = GH
  diffuseHz = GH - IdnH
END
