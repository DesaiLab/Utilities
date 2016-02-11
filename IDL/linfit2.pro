FUNCTION linfit2,a,b,_ref_extra=e,plot=plot,onetoone=onetoone
  good = where(finite(a) AND finite(b),ng)
  IF ng GT 0 THEN BEGIN 
    lf = ladfit(a[good],b[good],_extra=e)
    cr = correl(a,b)
    IF keyword_set(plot) THEN BEGIN
      IF ~keyword_set(onetoone) THEN BEGIN 
        comba = a[good]
        combb = b[good]
      ENDIF ELSE BEGIN
        comba = [a[good],b[good]]
        combb = comba
      ENDELSE 
      plot,a[good],b[good],xrange=[min(comba),max(comba)],yrange=[min(combb),max(combb)],psym=1,title='r2='+string(cr^2,format='(f0.2)')+' Y='+strtrim(string(lf[1]),2)+'*X+'+strtrim(string(lf[0]),2)

      IF keyword_set(onetoone) THEN oplot,[min(comba),max(comba)],[min(combb),max(combb)],linestyle=1,thick=3,color=100
      flinex = [min(comba),max(comba)]
      fliney = lf[0] + lf[1] * flinex
      oplot,flinex,fliney,linestyle=2,thick=2
    ENDIF     
    return,[lf,correl(a,b)]
  ENDIF ELSE BEGIN 
    return,[nan(),nan(),nan()]
  ENDELSE 
END
