FUNCTION box_whisker,x,y,horizontal=horizontal,stats=stats,symsize=symsize,psym=psym,color=color,bcolor=bcolor,width=width,fill=fill,fcolor=fcolor
;draw one box, assume plotting surface is setup
;y is an array of data, 

  IF keyword_set(stats) THEN BEGIN
    md = stats[0]
    mx = stats[1]
    mn = stats[2]
    fq = stats[3]
    tq = stats[4]
  ENDIF ELSE BEGIN
    md = median(y,/even)
    mx = max(y,/nan)
    mn = min(y,/nan)
    yel = n_elements(y)
    IF (yel MOD 2) NE 0 THEN fqloc = ((yel+1)/4)-1 ELSE fqloc = ((yel+2)/4)-1
    IF (yel MOD 2) NE 0 THEN tqloc = (((3*yel)+3)/4)-1 ELSE tqloc = (((3*yel)+2)/4)-1
    sy = y[sort(y)]
    fq = sy[fqloc]
    tq = sy[tqloc]
;    fq = median(y[where(y LT md)])
;    tq = median(y[where(y GT md)])
  ENDELSE 
  
  IF NOT keyword_set(psym) THEN psym = 2
  IF NOT keyword_set(width) THEN width = 0.33
  
  IF keyword_set(horizontal) THEN BEGIN
    IF keyword_set(fill) THEN box,tq,x-width,fq,x+width,color=fcolor 
    oplot,[mx,mn],[x,x],color=bcolor
    oplot,[mx,mx],[x-(width/2),x+(width/2)],color=bcolor
    oplot,[mn,mn],[x-(width/2),x+(width/2)],color=bcolor
    plots,md,x,psym=psym,symsize=symsize,color=color
    IF ~keyword_set(fill) THEN box,tq,x-width,fq,x+width,/outline,/nofill,color=bcolor   
  ENDIF ELSE BEGIN
    oplot,[x,x],[mx,mn],color=bcolor
    oplot,[x-(width/2),x+(width/2)],[mx,mx],color=bcolor
    oplot,[x-(width/2),x+(width/2)],[mn,mn],color=bcolor
    plots,x,md,psym=psym,symsize=symsize,color=color
    box,x-width,tq,x+width,fq,/outline,/nofill,color=bcolor
  ENDELSE 

  return,[md,mx,mn,fq,tq]

END
