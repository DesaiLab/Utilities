PRO Vector_Map, uv_vect, length = length, title = title, maglen = maglen, legend = legend
;display a [*,*,2] vector for vector_map with latlon

ll = get_surfacelatlon()
lats = transpose(ll[0,*,0])
lons = ll[*,0,1]

if not keyword_set(title) then title = ''

if not keyword_set(length) then length = 1.0
if keyword_set(maglen) then length = max( sqrt ( (uv_vect[*,*,0]^2) + (uv_vect[*,*,1]^2) ) ) / maglen

velovect, uv_vect[*,*,0],uv_vect[*,*,1],lons,lats,length=length,title=title,xtitle='Longitude',ytitle='Latitude'

if keyword_set(legend) then begin
  xyouts, -99.5, 33.8, '= '+legend, /noclip, charsize = 1.1
  lonsdiff = lons[1] - lons[0]
  latsdiff = lats[1] - lats[0]
  arrow, -99.55-lonsdiff, 33.85, -99.55, 33.85, /data
endif

END
