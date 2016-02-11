FUNCTION Get_Slp, srf,bounds=bounds
  ;First derivative of mapped surface
  ;returns slope of surface per meter

  ll = get_surfacelatlon(bounds=bounds)

  du = fltarr(n_elements(srf[*,0]),n_elements(srf[0,*]))
  dv = fltarr(n_elements(srf[*,0]),n_elements(srf[0,*]))

  for row = 0,n_elements(srf[*,0])-1 do begin
    lons = Sph_Dist_cumulative(ll[*,row,0],ll[*,row,1],units='meters')
    du[*,row] = deriv(lons,srf[*,row])
    ;we assume that lons is ordered from west to east (to get positive east), -100,-99,-98,...
    lats = Sph_Dist_cumulative(transpose(ll[row,*,0]),transpose(ll[row,*,1]),units='meters')
    dv[row,*] = transpose( deriv(lats,transpose(srf[row,*])))
    ;we assume that lats is ordered from south to north (to get positive north) - 34,35,36...
  endfor

  slp = fltarr(n_elements(du[*,0]),n_elements(du[0,*]),2)
  slp[*,*,0] = du[*,*]
  slp[*,*,1] = dv[*,*]
  return, slp
END
