FUNCTION PickVal, Srf, Lat, Lon, Sphere = Sphere, interp=interp,bounds=bounds
  ;Given a surface, find the Z value at a given Lat Lon

  IF NOT keyword_set(bounds) THEN b = [-99.4,34.3,-95.5,38.8] ELSE b = bounds
  nx = 26.0 ;grid size
  ny = 26.0 ;can't get any other size to work 
  gsx = (b[2] - b[0]) / (nx - 1.0)  ;grid width
  gsy = (b[3] - b[1]) / (ny - 1.0)
  x = (findgen(nx) * gsx) + b[0]     ;lons
  y = (findgen(ny) * gsy) + b[1]     ;lats
  
if keyword_set(interp) then begin
  valx = (lon - b[0]) / gsx
  valy = (lat - b[1]) / gsy
  val = interpolate(srf,valx,valy)
endif else begin
  If keyword_set(sphere) then begin  ;use sphereical distance
    pp = fltarr(nx,ny,2)
    for lonxpp = 0,nx-1 do begin
      for latypp = 0,ny-1 do begin
        pp[lonxpp,latypp,1] = x[lonxpp] 
        pp[lonxpp,latypp,0] = y[latypp]
      endfor
    endfor
    
    pi = 3.1415926  
    radconv = ( (2*pi) / 360.0)
    diff = abs(pp[*,*,1] - lon)
    diff = diff*radconv
    cosdiff = cos(diff)
    pp = pp * radconv
    latrad = lat * radconv
    lonrad = lon * radconv

    coslatrad = cos(latrad)
    cospplat = cos(pp[*,*,0])
    sinlatrad = sin(latrad)
    sinpplat = sin(pp[*,*,0])

    spheredis = (sinlatrad * sinpplat) + (coslatrad * cospplat * cosdiff)
    spheredis = acos(spheredis)
    spheredis = spheredis / radconv
    spheredis = spheredis * 69.172

    minsph = min(spheredis)
    loc = (where(spheredis eq minsph))[0]
    val = srf[loc]

  EndIf Else Begin

    xds = abs(x-lon)
    yds = abs(y-lat)

    xdsmin = min(xds)
    ydsmin = min(yds)
  
    xds = (where(xds eq xdsmin))[0]
    yds = (where(yds eq ydsmin))[0]

    Val = Srf[xds,yds]
  EndElse
endelse

  return, val
END
