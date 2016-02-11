FUNCTION Sph_Dist_Cumulative, lats, lons, units = units
  ;takes a list of lats,lons and finds distance from
  ;point 0-1, 0-1+1-2, 0-1+1-2+2-3, etc...
  ;easy way to find length of route
  
  spd = sph_dist_list(lats,lons,units=units)
  result = fltarr(n_elements(spd)+1)
  result[0] = 0.0
  for x = 0,n_elements(spd)-1 do begin
    result[x+1] = result[x] + spd[x]
  endfor
  return, result

END
