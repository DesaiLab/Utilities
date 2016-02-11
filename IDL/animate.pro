PRO Animate,v
  sz = size(v)
  IF sz[0] EQ 3 THEN BEGIN
    x = sz[1]
    y = sz[2]
    imgs = sz[3]
    xinteranimate,set=[x,y,imgs],/showload
    FOR i=0,imgs-1 DO xinteranimate,frame=i,image=v[*,*,i]
    xinteranimate,/keep_pixmaps
  ENDIF ELSE print, 'Bad array'
END
