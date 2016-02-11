FUNCTION Mapper, lat, lon, nm, dat, interpolate = interp, method = meth, noplot = noplot, nonum=nonum,title=title,bounds=bounds
;take old mapper prog
;map the string dat at lat and lon
;if keyword set interp, then do the smooth spline curve interpolation with interp data


;Basic function is to set projection and show output points

;b = [-100.0,34.0,-95.0,39.0]
IF NOT keyword_set(bounds) THEN b = [-99.4,34.3,-95.5,38.8] ELSE b = bounds  ;same bounds as AVHRR image
cenlon = (b[0] + b[2]) / 2.0
cenlat = (b[1] + b[3]) / 2.0

If not keyword_set(noplot) then Begin
  if not keyword_set(title) then title = ''
  Map_Set, cenlat,cenlon,limit = [b[1],b[0],b[3],b[2]], /transverse_mercator,/advance,title=title,/isotropic
  Map_Grid, label = 1, lats = [34.0,35.0,36.0,37.0,38.0,39.0], lons = [-100.0,-99.0,-98.0,-97.0,-96.0,-95.0]
  xyouts, lon, lat, strtrim(nm,2)
  if not keyword_set(nonum) then xyouts, lon, lat-0.1, strtrim(string(dat),2)
EndIf

;Now we want to add a contour map on top

if keyword_set(interp) then begin

  nx = 26.0 ;grid size
  ny = 26.0 ;can't get any other size to work 
  gsx = (b[2] - b[0]) / (nx - 1.0)  ;grid width
  gsy = (b[3] - b[1]) / (ny - 1.0)
  minx = b[0]  ;start lon
  miny = b[1]  ;start lat
  if not keyword_set(meth) then meth = 2  ;default method is kriging
  if meth eq 0 then meth = 3

;Choose interpolation method
  Case meth Of
    3  : Begin  ;Spherical triangulation
           srf = sph_scat(lon,lat,dat,nlat=nx,nlon=ny,bounds=b)   
         End
    1  : Begin  ;Spline trend surface
           srf = min_curve_surf(dat, lon, lat,bounds=b) 
         End
    2  : Begin  ;Kriging with exponential semiovariogram
           srf = krig2d(dat,lon,lat,bounds=b, expon = [4.0,0.0])
         End
    4  : Begin  ;Nearest neighbor surface
           srf = nearest_neighbor(lon,lat,dat,bounds=b)
         End
  EndCase  


;Draw contour map

  x = (findgen(nx) * gsx) + minx     ;lons
  y = (findgen(ny) * gsy) + miny     ;lats
  If Not Keyword_set(noplot) Then Begin
    contour, srf, x, y,  /C_ANNOTATION, NLEVELS = 20, /overplot  ;LEVELS = findgen(15) * 50.0, /overplot
  EndIf

  return, srf
endif else return, 0.0

END
