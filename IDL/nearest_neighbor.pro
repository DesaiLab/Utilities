FUNCTION Nearest_Neighbor, lon, lat, z,bounds=bounds
  ;function nearest_neigh
  ;set each grid point to the closeest call - slow algorithm!  
pp = get_surfacelatlon(bounds=bounds)
srf = fltarr(n_elements(pp[*,0,0]),n_elements(pp[0,*,0]))
for la = 0,n_elements(pp[0,*,0])-1 do begin
  for lo = 0,n_elements(pp[*,0,1])-1 do begin
    thela = pp[lo,la,0]
    thelo = pp[lo,la,1]
    dis = sph_dist(thela,thelo,lat,lon,units='arc')
    wheremindis = (where(dis eq min(dis)))[0]
    srf[lo,la] = z[wheremindis]
  endfor
endfor

return, srf

END
