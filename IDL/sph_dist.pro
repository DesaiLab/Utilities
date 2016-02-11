FUNCTION Sph_Dist, lat1, lon1, lat2, lon2, units = units
;from http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1
  pi = 3.1415926  
  radconv = ( (2*pi) / 360.0)
 
  lat1rad = lat1 * radconv
  lat2rad = lat2 * radconv
  lon1rad = lon1 * radconv
  lon2rad = lon2 * radconv

  dlon = lon2rad - lon1rad
  dlat = lat2rad - lat1rad

  ;dlon = abs(lon2 - lon1) * radconv
  ;dlat = abs(lat2 - lat1) * radconv

  a = (sin(dlat/2))^2 + cos(lat1rad) * cos(lat2rad) * (sin(dlon/2))^2 
  ;c = 2 * asin(min([1,sqrt(a)]))  ;another way of doing it
  c = 2 * atan( sqrt(a), sqrt(1-a) )
  cd = c ;/ radconv

  if not keyword_set(units) then units = 'miles'
  if units eq 'meters' then ka = 6378.0 * 1000.0
  if units eq 'miles' then ka = 3963.0
  if units eq 'kilometers' then ka = 6378.0 
  if units eq 'arc' then ka = 1.0

  e = 0.081082
  lat = mean([lat1rad,lat2rad])
  R = ka * sqrt(1 - e^2) / (1 - e^2 * (sin(lat))^2) 

  if units eq 'arc' then R = 1.0

  d = R * cd 
  ;print, 'R is ',R,' and c is ',cd
  return , d

END

FUNCTION Sph_Dist_Old, lat1, lon1, lat2, lon2, units = units
;computes spherical distance between lats1,lons1 to lats2,lons2

    pi = 3.1415926  
    radconv = ( (2*pi) / 360.0)
  
    diff = abs(lon2 - lon1)
    diff = diff*radconv
    cosdiff = cos(diff)

    lat1rad = lat1 * radconv
    lat2rad = lat2 * radconv
    lon1rad = lon1 * radconv
    lon2rad = lon2 * radconv

    coslat1rad = cos(lat1rad)
    coslat2rad = cos(lat2rad)
    sinlat1rad = sin(lat1rad)
    sinlat2rad = sin(lat2rad)

    spheredis = (sinlat1rad * sinlat2rad) + (coslat1rad * coslat2rad * cosdiff)
    spheredis = acos(spheredis)
    spheredis = spheredis / radconv

    if keyword_set(units) then begin
      if units eq 'miles' then spheredis = spheredis * 69.172
      if units eq 'meters' then spheredis = spheredis * 111177.0
      if units eq 'kilometers' then spheredis = spheredis * 111.177
      if units eq 'arc' then spheredis = spheredis
    endif else begin
      spheredis = spheredis * 69.172
    endelse

    return, spheredis
END
