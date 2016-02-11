FUNCTION daynight,startdoy,enddoy,yr=yr,interval=interval,rise=rise,set=set,noon=noon,lat=lat,lon=lon,utc=utc,timezone=timezone,doplot=doplot,doshortwave=doshortwave,$
                  pressureMb=pressureMb, ozoneCm=ozoneCm, waterCm=WaterCm, AOD500nm=AOD500nm, AOD380nm=AOD380nm, Ba=Ba, albedo=albedo, $
                  directBeam=directBeam, directHz=directHz, globalHz=globalHz, diffuseHz=diffuseHz, birdETR=birdETR, $
                  solarazimuth=solarazimuth, solarelevation=solarelevation, solarzenith=solarzenith

;when is it day or night?
;returns an array with interval points per day
;from startdoy to enddoy 
;0 for night, 1 for day, 2 for solar noon
;total array size = interval * (enddoy-startdoy+1)

;More specifically: 1 means that the sun is up for some part of that
;time interval, 0 means that the sun did not rise or set in the time
;interval, 2 means solar noon occurred in this time interval

;if you don't provide startdoy or enddoy then startdoy and enddoy
;go from 1 to days_in_year

;More on output (example): to find when is it night in your array
;and set those values to 0.0:
;
;dn = daynight(1,365,yr=2003,interval=48,lat=45.0,lon=-90.0)
;night = where(dn eq 0)
;daytime = where(dn ge 1)
;array[night] = 0.0

;default output is in Central Standard Time (-6 from UTC) unless
;  1.) UTC keyword is set -OR-
;  2.) keyword timezone is set to the difference between local and UTC
;default yr is current year (from computer clock)
;default interval is 48 (48 points per day = every 30 minutes)
;default lat and lon is Sylvania flux tower
;Negative lon is west
;Positive lat is north

;Keywords rise, set and noon contain arrays with the
;Sunrise, sunset and solar noon for each day
;from startdoy to enddoy

;if Keyword doshortwave is set, then the following will be calculated:
;  directBeam - direct beam solar radiation - normal to beam
;  directHz - direct solar radiation on a horizontal surface
;  diffuzeHz - diffuse solar radiation on a horizontal suface
;  globalHz - total (direct+diffuse) solar radiation on a horizontal surface
;  birdETR - model of solar constant (extraterrestrial radiation)
;  solarazimuth, solarelevation, solarzenith - solar position in degrees
;
;  These arrays have the same length as daynight and have interval
;  points per day in the same time zone as daynight
;
;  Example:
;  dn = daynight(150,200,yr=2003,interval=24,albedo=0.2,globalHz=g,solarze=z,/doshort)
;
;  keywords pressureMb, ozoneCm, waterCm, AOD500nm, AOD380nm, Ba, and
;  albedo are inputs to the shortwave model - see birdandhultstrom.pro
;  for more information

;keyword doplot will make a plot showing sunset,sunrise and solar noon

;Setup start and end day, figure out days in year
  caldat,systime(/julian),mo,day,year
  IF NOT keyword_set(yr) THEN yr = year
  lp = ((((Yr mod 4) eq 0) and ((yr mod 100) ne 0)) or ((yr mod 400) eq 0))
  IF lp THEN days_in_year = 366 ELSE days_in_year = 365

  IF NOT keyword_set(startdoy) THEN BEGIN
    startdoy = 1
    enddoy = days_in_year 
  ENDIF ELSE IF NOT keyword_set(enddoy) THEN enddoy = days_in_year 

  ndays = enddoy - startdoy + 1

  IF NOT keyword_set(interval) THEN interval = 48 ;point every 30 minutes

;Setup sunrise/sunset arrays
  tmarr = intarr(interval)
  rise = fltarr(ndays)
  set = rise
  noon = rise
  daynight = intarr(long(ndays)*long(interval))
  IF n_elements(timezone) NE 0 THEN subval = timezone  ELSE subval = -6.0
  IF keyword_set(utc) THEN subval = 0.0

;Setup arrays for calculating shortwave fluxes and solar position
  IF keyword_set(doshortwave) THEN BEGIN 
    yrarr = make_array(n_elements(daynight),/float,value=yr)
    doyarr = rebin(dindgen(ndays)+startdoy,n_elements(daynight),/sample)
    tmarr = ((dindgen(n_elements(daynight)) MOD interval)/interval)
    fjday = doyarr+tmarr
    fjday_utc = fjday-(subval/24.0d)
    lstyr = where(fjday_utc LT 1.0,nly)
    IF nly GT 0 THEN BEGIN
      yrarr[lstyr] = yr - 1
      lplstyr = (((((Yr-1) mod 4) eq 0) and (((yr-1) mod 100) ne 0)) or (((yr-1) mod 400) eq 0))
      IF lplstyr THEN diyl = 366 ELSE diyl = 365
      fjday_utc[lstyr] = fjday_utc[lstyr]+diyl
    ENDIF
    nxtyr = where(fjday_utc GE (days_in_year+1.0),nny)
    IF nny GT 0 THEN BEGIN
      yrarr[nxtyr] = yr + 1
      fjday_utc[nxtyr] = fjday_utc[nxtyr]-days_in_year
    ENDIF
    doyarr = float(fix(float(fjday_utc)))
    tmarr = float((fjday_utc - doyarr)*24.0)
    hharr = float(fix(tmarr))
    mmarr = (tmarr-hharr)*60.0
    ssarr = (mmarr-fix(mmarr))*60.0
    directBeam = make_array(n_elements(daynight),/float,value=!values.f_nan)
    directHz = directBeam
    globalHz = directBeam
    diffuseHz = directBeam
    birdETR = directBeam
    solarazimuth = directBeam
    solarelevation = directBeam
    solarzenith = directBeam
  ENDIF 

;Calculate sunrise/sunset and shortwave fluxes
  FOR i = startdoy,enddoy DO BEGIN
    ss = sunrise(i,yr,lat=lat,lon=lon)+subval

    arrloc = i-startdoy
    rise[arrloc] = ss[0]
    set[arrloc] = ss[1]
    noon[arrloc] = ss[2]

    startrise = fix(rise[arrloc] * (interval/24.0)) 
    endrise = fix(set[arrloc] * (interval/24.0))   
    IF finite(rise[arrloc]) AND ~finite(set[arrloc]) THEN endrise = fix(24. * interval/24.0)
    IF finite(set[arrloc]) AND ~finite(rise[arrloc]) THEN startrise = 0 
    IF ~finite(set[arrloc]) AND ~finite(rise[arrloc]) THEN BEGIN
      locstart = ((arrloc*interval)+0) > 0 < ((interval*ndays)-1)
      locend = (((arrloc+1)*interval)-1) > 0 < ((interval*ndays)-1)
      IF locend GE locstart THEN BEGIN
        IF (i LT (80+lp)) OR (i GT (265+lp)) THEN BEGIN
          daynight[locstart:locend] = 0
          noon[arrloc] = !values.f_nan
        ENDIF ELSE daynight[locstart:locend] = 1
      ENDIF 
    ENDIF ELSE BEGIN 
      locstart = ((arrloc*interval)+startrise+1) > 0 < ((interval*ndays)-1)
      locend = (arrloc*interval)+endrise > 0 < ((interval*ndays)-1)
      IF locend GE locstart THEN daynight[locstart:locend] = 1
    ENDELSE 
    
    IF finite(noon[arrloc]) THEN BEGIN
      closenoon = fix(noon[arrloc] * (interval/24.0))
      locnoon = (arrloc*interval)+closenoon
      IF (locnoon GE 0) AND (locnoon LT (interval*ndays)) THEN daynight[locnoon] = 2 
    ENDIF 

    IF keyword_set(doShortwave) THEN BEGIN 
      FOR z = arrloc*interval,(arrloc*interval)+(interval-1) DO BEGIN 
        BirdAndHulstrom,doyarr[z], hharr[z], mmarr[z], ssarr[z], yr=yrarr[z], pressureMb=pressureMb, $
          ozoneCm=ozoneCm, waterCm=WaterCm, AOD500nm=AOD500nm, $
          AOD380nm=AOD380nm, Ba=Ba, albedo=albedo, $
          directBeam=dB, directHz=dH, globalHz=gH, $
          diffuseHz=dfH, birdETR=bE, lat=lat, lon=lon, $
          solarazimuth=s_az, solarelevation=s_el, solarzenith=s_ze
        directBeam[z] = dB
        directHz[z] = dH
        globalHz[z] = gH
        diffuseHz[z] = dfH
        birdETR[z] = bE
        solarazimuth[z] = s_az
        solarelevation[z] = s_el
        solarzenith[z] = s_ze
      ENDFOR 
    ENDIF 
  ENDFOR

  IF keyword_set(doplot) THEN BEGIN
    doyarr = indgen(ndays)+startdoy
    plot,doyarr,rise,title='Sunrise,Noon,Sunset',yrange=[0,(max(fix(set))+1)>24],ytitle='Hour',xtitle='DOY',xrange=[1,days_in_year],xticks=11,xtickv=[1,32,60,91,121,152,182,213,244,274,305,335],xtickn=['J','F','M','A','M','J','J','A','S','O','N','D']
    oplot,doyarr,set
    oplot,doyarr,noon
  ENDIF

  return,daynight

END
