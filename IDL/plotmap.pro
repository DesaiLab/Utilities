PRO PlotMap,lats,lons,text=text,title=title,b_lat=b_lat,b_lon=b_lon,b_margin=b_margin,b_bounds=b_bounds,over=over,thick=thick,color=color,psym=psym,nodata=nodata,linestyle=linestyle,symsize=symsize,b_nomargin=b_nomargin,b_avhrr=b_avhrr,alignment=alignment,charsize=charsize,charthick=charthick,orientation=orientation,plot_grid=plot_grid,font=font

  IF NOT keyword_set(over) THEN BEGIN
   
    b = [0.0,0.0,0.0,0.0]

    IF NOT keyword_set(b_nomargin) THEN BEGIN
      IF NOT keyword_set(b_margin) THEN b_margin = 0.5
      b[1] = round(min(lats))-b_margin
      b[3] = round(max(lats))+b_margin
      b[0] = round(min(lons))-b_margin
      b[2] = round(max(lons))+b_margin
    ENDIF ELSE b =[min(lons),min(lats),max(lons),max(lats)]

    IF keyword_set(b_avhrr) THEN b = [-99.5,34.5,-95.5,38.5]
    IF keyword_set(b_bounds) THEN b = b_bounds
    IF keyword_set(b_lat) THEN BEGIN
      b[1] = min(b_lat)
      b[3] = max(b_lat)
    ENDIF 
    IF keyword_set(b_lon) THEN BEGIN
      b_lon = abs(b_lon) * (-1.0)
      b[0] = min(b_lon)
      b[2] = max(b_lon)
    ENDIF

    simplemap,title=title,bounds=b,font=font
  ENDIF

  IF NOT keyword_set(nodata) THEN BEGIN
    IF keyword_set(text) THEN IF keyword_set(color) THEN xyouts,lons,lats,text,charthick=charthick,charsize=charsize,alignment=alignment,color=color,font=font ELSE xyouts,lons,lats,text,charthick=charthick,charsize=charsize,alignment=alignment,noclip=0,font=font ELSE oplot,lons,lats,thick=thick,color=color,linestyle=linestyle,psym=psym,symsize=symsize
 ENDIF

 IF keyword_set(plot_grid) THEN plot_latlon,bounds=b,gridsize=plot_grid

END
