FUNCTION Get_SurfaceLatLon,bounds=bounds,gridsize=gridsize
  ;compute a matrix of the latitude and longitude
  IF NOT keyword_set(bounds) THEN b = [-99.4,34.3,-95.5,38.8] ELSE b = bounds
  IF NOT keyword_set(gridsize) THEN gridsize = 26.0
  nx = gridsize ;grid size
  ny = gridsize ;can't get any other size to work 
  gsx = (b[2] - b[0]) / (nx - 1.0)  ;grid width
  gsy = (b[3] - b[1]) / (ny - 1.0)
  x = (findgen(nx) * gsx) + b[0]     ;lons
  y = (findgen(ny) * gsy) + b[1]     ;lats
  pp = fltarr(nx,ny,2)
  for lonxpp = 0,nx-1 do begin
    for latypp = 0,ny-1 do begin
      pp[lonxpp,latypp,1] = x[lonxpp] 
      pp[lonxpp,latypp,0] = y[latypp]
    endfor
  endfor
  return, pp
END
