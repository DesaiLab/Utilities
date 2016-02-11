PRO Plot_LatLon,bounds=bounds,gridsize=gridsize
  ;display the grid of lat lon
  IF NOT keyword_set(gridsize) THEN gridsize = 1
  IF gridsize EQ 1 THEN gridsize = 0
  pp = Get_SurfaceLatLon(bounds=bounds,gridsize=gridsize)
  oplot, pp[*,*,1], pp[*,*,0], psym = 3
END
