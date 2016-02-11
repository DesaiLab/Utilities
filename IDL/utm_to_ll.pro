FUNCTION UTM_to_LL,ReferenceEllipsoid=ReferenceEllipsoid,easts,norths,zones

;converts UTM coords to lat/long.  Equations from USGS Bulletin 1532 
;Written by Chuck Gantz- chuck.gantz@globalstar.com
;Ported to IDL by Ankur Desai

  PI = 3.14159265D
  FOURTHPI = PI / 4
  deg2rad = PI / 180.0D
  rad2deg = 180.0D / PI
  If not keyword_set(ReferenceEllipsoid) Then ReferenceEllipsoid = 'wgs84'

  k0 = 0.9996D
  a = (ellip(ReferenceEllipsoid))[0] ;EquatorialRadius
  eccSquared = (ellip(ReferenceEllipsoid))[1] ;eccentricitySquared
  e1 = ((1.0D)-sqrt((1.0D)-eccSquared))/((1.0D)+sqrt((1.0D)-eccSquared))

  nelem = min([n_elements(easts),n_elements(norths),n_elements(zones)])
  lats = dblarr(nelem)
  lons = dblarr(nelem)

  FOR manyutm = 0,nelem-1 DO begin
    UTMEasting = easts[manyutm]
    UTMNorthing = norths[manyutm]
    UTMZone = zones[manyutm]

    x = double(UTMEasting) - (500000.0D) ;remove 500,000 meter offset for longitude
    y = double(UTMNorthing)

    UZ = strupcase(strtrim(strcompress(UTMZone,/remove_all),2))
    ZoneNumber = long(UZ)
    ZoneLetter = strmid(UZ,strlen(UZ)-1,1)

    if (ZoneLetter ge 'N') then NorthernHemisphere = 1 else begin
      NorthernHemisphere = 0
      y = y -  (10000000.0D)
    endelse

    LongOrigin = (double(ZoneNumber) - (1.0D))*(6.0D) - (180.0D) + (3.0D) ;+3 puts origin in middle of zone

    eccPrimeSquared = (eccSquared)/((1.0D)-eccSquared)

    M = y / k0
    mu = M/(a*((1.0D)-eccSquared/(4.0D)-(3.0D)*eccSquared*eccSquared/(64.0D)-(5.0D)*eccSquared*eccSquared*eccSquared/(256.0D)))
    phi1Rad = mu+((3.0D)*e1/(2.0D)-(27.0D)*e1*e1*e1/(32.0D))*sin((2.0D)*mu)+((21.0D)*e1*e1/(16.0D)-(55.0D)*e1*e1*e1*e1/(32.0D))*sin((4.0D)*mu)+((151.0D)*e1*e1*e1/(96.0D))*sin((6.0D)*mu)
    phi1 = phi1Rad*rad2deg

    N1 = a/sqrt((1.0D)-eccSquared*sin(phi1Rad)*sin(phi1Rad))
    T1 = tan(phi1Rad)*tan(phi1Rad)
    C1 = eccPrimeSquared*cos(phi1Rad)*cos(phi1Rad)
    R1 = a*((1.0D)-eccSquared)/  (((1.0D)-eccSquared*sin(phi1Rad)*sin(phi1Rad))^(1.5D))
    D = x/(N1*k0)

    Lat = phi1Rad-(N1*tan(phi1Rad)/R1)*(D*D/(2.0D)-((5.0D)+(3.0D)*T1+(10.0D)*C1-(4.0D)*C1*C1-(9.0D)*eccPrimeSquared)*D*D*D*D/(24.0D)+((61.0D)+(90.0D)*T1+(298.0D)*C1+(45.0D)*T1*T1-(252.0D)*eccPrimeSquared-(3.0D)*C1*C1)*D*D*D*D*D*D/(720.0D))
    Lat = Lat * rad2deg

    Long = (D-((1.0D)+(2.0D)*T1+C1)*D*D*D/(6.0D)+((5.0D)-(2.0D)*C1+(28.0D)*T1-(3.0D)*C1*C1+(8.0D)*eccPrimeSquared+(24.0D)*T1*T1)*D*D*D*D*D/(120.0D))/cos(phi1Rad)
    Long = LongOrigin + Long * rad2deg

;   Return,[Lat,Long]
    lats[manyutm] = lat
    lons[manyutm] = long
  ENDFOR

  IF nelem eq 1 THEN return,[lats[0],lons[0]] ELSE return,[transpose(lats),transpose(lons)]

END
