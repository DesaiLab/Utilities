FUNCTION UTMLetterDesignator,Lat

;This routine determines the correct UTM letter designator for the given latitude
;returns 'Z' if latitude is outside the UTM limits of 84N to 80S
;Written by Chuck Gantz- chuck.gantz@globalstar.com
;Ported to IDL by Ankur Desai

  LetterDesignator = 'Z'
  if ((84.0 ge Lat) and (Lat ge 72.0)) Then LetterDesignator = 'X'
  if ((72.0 gt Lat) and (Lat ge 64.0)) Then LetterDesignator = 'W'
  if ((64.0 gt Lat) and (Lat ge 56.0)) Then LetterDesignator = 'V'
  if ((56.0 gt Lat) and (Lat ge 48.0)) Then LetterDesignator = 'U'
  if ((48.0 gt Lat) and (Lat ge 40.0)) Then LetterDesignator = 'T'
  if ((40.0 gt Lat) and (Lat ge 32.0)) Then LetterDesignator = 'S'
  if ((32.0 gt Lat) and (Lat ge 24.0)) Then LetterDesignator = 'R'
  if ((24.0 gt Lat) and (Lat ge 16.0)) Then LetterDesignator = 'Q'
  if ((16.0 gt Lat) and (Lat ge 8.0)) Then LetterDesignator = 'P'
  if (( 8.0 gt Lat) and (Lat ge 0.0)) Then LetterDesignator = 'N'
  if (( 0.0 gt Lat) and (Lat ge -8.0)) Then LetterDesignator = 'M'
  if ((-8.0 gt Lat) and (Lat ge -16.0)) Then LetterDesignator = 'L'
  if ((-16.0 gt Lat) and (Lat ge -24.0)) Then LetterDesignator = 'K'
  if ((-24.0 gt Lat) and (Lat ge -32.0)) Then LetterDesignator = 'J'
  if ((-32.0 gt Lat) and (Lat ge -40.0)) Then LetterDesignator = 'H'
  if ((-40.0 gt Lat) and (Lat ge -48.0)) Then LetterDesignator = 'G'
  if ((-48.0 gt Lat) and (Lat ge -56.0)) Then LetterDesignator = 'F'
  if ((-56.0 gt Lat) and (Lat ge -64.0)) Then LetterDesignator = 'E'
  if ((-64.0 gt Lat) and (Lat ge -72.0)) Then LetterDesignator = 'D'
  if ((-72.0 gt Lat) and (Lat ge -80.0)) Then LetterDesignator = 'C'

  return,LetterDesignator
END

FUNCTION LL_to_UTM,ReferenceEllipsoid=ReferenceEllipsoid,Lats,Lons,UTMZone=UTMZone

;Equations from USGS Bulletin 1532 
;C++ code written by Chuck Gantz- chuck.gantz@globalstar.com
;Ported to IDL by Ankur Desai

  PI = 3.14159265D
  FOURTHPI = PI / 4
  deg2rad = PI / 180.0D
  rad2deg = 180.0D / PI
  If not keyword_set(ReferenceEllipsoid) Then ReferenceEllipsoid = 'clarke1866'

  nelem = min([n_elements(lats),n_elements(lons)])
  utme = dblarr(nelem)
  utmn = dblarr(nelem)
  utmzone = strarr(nelem)

  FOR manylats = 0,nelem-1 DO BEGIN

    Long = Double(Lons[manylats])
    Lat = Double(Lats[manylats])

    a = (ellip(ReferenceEllipsoid))[0] ;EquatorialRadius
    eccSquared = (ellip(ReferenceEllipsoid))[1] ;eccentricitySquared
    k0 = 0.9996D
    ;Make sure the longitude is between -180.00 .. 179.9
    
    LongTemp = double((Long+(180.0D))-fix((Long+(180.0D))/(360.0D))*(360.0D)-(180.0D))
    LatRad = Lat*deg2rad        ;
    LongRad = LongTemp*deg2rad  ;
    
    ZoneNumber = fix((LongTemp + (180.0D))/(6.0D)) + 1 ;
    
    if (Lat ge 56.0) and (Lat lt 64.0) and (LongTemp ge 3.0) and (LongTemp lt 12.0) then ZoneNumber = 32
    if (Lat ge 72.0) and (Lat lt 84.0) then begin 
      if (LongTemp ge 0.0)  and (LongTemp lt  9.0) then ZoneNumber = 31
      if (LongTemp ge 9.0)  and (LongTemp lt 21.0) then ZoneNumber = 33
      if (LongTemp ge 21.0) and (LongTemp lt 33.0) then ZoneNumber = 35
      if (LongTemp ge 33.0) and (LongTemp lt 42.0) then ZoneNumber = 37
    endif

    LongOrigin = double((ZoneNumber - 1)*6 - 180 + 3) ;+3 puts origin in middle of zone
    LongOriginRad = LongOrigin * deg2rad ;

    UTMZone[manylats] = strtrim(string(ZoneNumber),2) + UTMLetterDesignator(Lat)

    eccPrimeSquared = (eccSquared) / ((1.0D) - eccSquared)

    N = a / sqrt((1.0D)-eccSquared*sin(LatRad)*sin(LatRad))
    T = tan(LatRad)*tan(LatRad)
    C = eccPrimeSquared*cos(LatRad)*cos(LatRad)
    AA = cos(LatRad)*(LongRad-LongOriginRad)

    M = a*(((1.0D)-eccSquared/(4.0D)-(3.0D)*eccSquared*eccSquared/(64.0D)-(5.0D)*eccSquared*eccSquared*eccSquared/(256.0D))*LatRad-((3.0D)*eccSquared/(8.0D)+(3.0D)*eccSquared*eccSquared/(32.0D)+(45.0D)*eccSquared*eccSquared*eccSquared/(1024.0D))*sin((2.0D)*LatRad)+((15.0D)*eccSquared*eccSquared/(256.0D)+(45.0D)*eccSquared*eccSquared*eccSquared/(1024.0D))*sin((4.0D)*LatRad)-((35.0D)*eccSquared*eccSquared*eccSquared/(3072.0D))*sin((6.0D)*LatRad))
    
    UTMEasting = double((k0*N*(AA+((1.0D)-T+C)*AA*AA*AA/(6.0D)+((5.0D)-(18.0D)*T+T*T+(72.0D)*C-(58.0D)*eccPrimeSquared)*AA*AA*AA*AA*AA/(120.0D)) + (500000.0D)))
    UTMNorthing = double((k0*(M+N*tan(LatRad)*(AA*AA/(2.0D)+((5.0D)-T+(9.0D)*C+(4.0D)*C*C)*AA*AA*AA*AA/(24.0D) + ((61.0D)-(58.0D)*T+T*T+(600.0D)*C-(330.0D)*eccPrimeSquared)*AA*AA*AA*AA*AA*AA/(720.0D)))))

    if (Lat lt 0.0) Then UTMNorthing = UTMNorthing + (10000000.0D) ;10000000 meter offset for southern hemisphere

    utme[manylats] = UTMEasting
    utmn[manylats] = UTMNorthing
  ENDFOR

  IF nelem EQ 1 THEN utmzone = utmzone[0]

  IF nelem eq 1 THEN return,[utme[0],utmn[0]] ELSE return,[transpose(utme),transpose(utmn)]

;  Return,[UTMEasting,UTMNorthing]

END
