FUNCTION Sph_Dist_List, lats, lons, units = units
  ;takes a list of lats,lons and finds distance from
  ;point 0-1, 1-2, 2-3, etc...
  ;returns array size lats-1

  p = min([n_elements(lats),n_elements(lons)])
  p = p - 2
  lats1 = lats
  lats2 = shift(lats,-1)
  lons1 = lons
  lons2 = shift(lons,-1)

  return, sph_dist(lats1[0:p],lons1[0:p],lats2[0:p],lons2[0:p],units=units)  
END
