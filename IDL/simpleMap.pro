PRO SimpleMap, title=title,bounds=bounds,font=font
;just set up a projection
IF NOT keyword_set(bounds) THEN b = [-99.4,34.3,-95.5,38.8] ELSE b = bounds  ;same bounds as AVHRR image
cenlon = (b[0] + b[2]) / 2.0
cenlat = (b[1] + b[3]) / 2.0
if not keyword_set(title) then title = ''
Map_Set, cenlat,cenlon,limit = [b[1],b[0],b[3],b[2]], /transverse_mercator, title = title,/isotropic
Map_Grid, label = 1, lats = [34.0,35.0,36.0,37.0,38.0,39.0], lons = [-100.0,-99.0,-98.0,-97.0,-96.0,-95.0],font=font
END
