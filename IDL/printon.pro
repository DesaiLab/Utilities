PRO PrintOn,f, nodir = nodir, noext = noext,ysize=ysize,xsize=xsize,color=color,landscape=landscape, font_size = font_size,bits_per_pixel=bits_per_pixel,nocolor=nocolor,xoffset=xoffset,yoffset=yoffset
  set_plot, 'ps'
  fl = f
  if not keyword_set(nodir) then BEGIN
    defsysv,'!PLOTDIR',exists = plotdiryes
    IF plotdiryes EQ 1 THEN BEGIN
      IF !PLOTDIR EQ 'SGP' THEN fl = sgp_basedir('plots')+f ELSE BEGIN
        IF !PLOTDIR EQ 'CO2' THEN fl = co2_basedir('plots')+f ELSE BEGIN
          fl = !PLOTDIR+f
        ENDELSE
      ENDELSE
    ENDIF ELSE fl = co2_basedir('plots')+f
  ENDIF
  if not keyword_set(noext) then fl = fl + '.ps'
  if not keyword_set(ysize) then ysize = 7.5
  if not keyword_set(xsize) then xsize = ysize < 7.5
  IF NOT keyword_set(fontsize) THEN fontsize = 10.0
  IF NOT keyword_set(bits_per_pixel) THEN bits_per_pixel = 8
  IF NOT keyword_set(nocolor) THEN color = 1b
  IF n_elements(yoffset) EQ 0 THEN yoffset = (11.0 - ysize) / 2.0
  IF n_elements(xoffset) EQ 0 THEN xoffset = (8.5 - xsize) / 2.0 
  
  IF NOT keyword_set(landscape) THEN BEGIN
    device, filename = fl, /inches, yoffset = yoffset, ysize = ysize, xoffset = xoffset, xsize = xsize, color = color, landscape = landscape, font_size = font_size, /helv,bits_per_pixel=bits_per_pixel 
  ENDIF ELSE BEGIN
    device, filename=fl,color=color,/landscape,font_size=font_size,/helv,bits_per_pixel=bits_per_pixel
  ENDELSE
  
  !p.font = 0
  
  IF keyword_set(landscape) THEN device,/landscape
  
END
