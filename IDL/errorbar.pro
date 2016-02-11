PRO errorbar,x,y,pos,neg,fillcolor=fillcolor,pattern=pattern,line_fill=line_fill,spacing=spacing,linecolor=linecolor,_EXTRA=_EXTRA,over=over,noadd=noadd,noline=noline
  IF n_elements(neg) EQ 0 THEN neg = pos
  
  polyx = [x,reverse(x)]
  IF keyword_set(noadd) THEN polyy = [pos,reverse(neg)] ELSE polyy = [y+pos,reverse(y-neg)]
  
  IF n_elements(fillcolor) EQ 0 THEN fillcolor = 180

  IF NOT keyword_set(over) THEN plot,x,y,/nodata,_extra=_extra
  polyfill,polyx,polyy,color=fillcolor,pattern=pattern,line_fill=line_fill,spacing=spacing,noclip=0
  IF NOT keyword_set(noline) THEN oplot,x,y,_extra=_extra

END
