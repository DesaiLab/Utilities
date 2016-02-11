PRO box,ux,uy,lx,ly,_extra=_extra,outline=outline,nofill=nofill,outcolor=outcolor,data=data,device=device,normal=normal,outthick=outthick
;Draw a box from ux,uy to lx,ly
  xs = [ux,ux,lx,lx,ux]
  ys = [uy,ly,ly,uy,uy]
  IF NOT keyword_set(nofill) THEN polyfill,xs,ys,_extra=_extra,data=data,device=device,normal=normal
  IF keyword_set(outline) THEN BEGIN
    plots,xs,ys,color=outcolor,data=data,device=device,normal=normal,thick=outthick
  ENDIF
END
