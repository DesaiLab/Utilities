FUNCTION Inverse_Distance,lats,lons,dats,lat,lon,radius=radius,exponent=exponent
;interpolate to a lat,lon given a set of lats,lons and datavalues
;radius is radius set at meters, default it to use all values
;default exponent is 2
;uses great circle distance

count = 0
nelem = [n_elements(lats)-1,n_elements(lons)-1]
nelem = min(nelem)
goodarr = bytarr(nelem+1)
distarr = fltarr(nelem+1)
IF NOT keyword_set(radius) THEN radius = !VALUES.F_INFINITY
IF NOT keyword_set(exponent) THEN exponent = 2.0
FOR c = 0,nelem DO BEGIN
  dst = sph_dist(lats[c],lons[c],lat,lon,units='meters')
  distarr[c] = dst / 1000.0
  IF ((dst GT 0.0) AND (dst LE radius))  THEN BEGIN 
    goodarr[c] = 1
    count = count + 1
  ENDIF
  IF (dst EQ 0.0) THEN goodarr[c] = 2
  IF (dats[c] LE -99.0) THEN goodarr[c] = 0
ENDFOR

n = where(goodarr EQ 2,ct)
IF ct GT 0 THEN return,dats[n] ELSE BEGIN
  IF count GT 0 THEN BEGIN
    s = where(goodarr EQ 1,cnt)
    gooddist = distarr[s]
    gooddat = dats[s]
    gooddist = double(gooddist ^ exponent)
    top = total (gooddat / gooddist,/double)
    bottom = total (1.0D / gooddist,/double)
    return,top/bottom
  ENDIF ELSE return,-999.0
ENDELSE

END
