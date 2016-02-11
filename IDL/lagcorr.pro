FUNCTION lagcorr,x,y,lags=lags,avg=avg,pval=pval,dof=dof,normalize=normalize,grange=grange,cause=cause,glag=glag,sig=sig

;does x perdict y?

;allow normalization?  (divide by multi-year sd?)

;resampling or mean replacement?

  IF n_elements(y) EQ 0 THEN y=x  ;autocorrelation

  nx = n_elements(x)
  ny = n_elements(y)
  nel = min([nx,ny])

  IF nel LT 11 THEN BEGIN
    print,'11 elements minimum!'
    stop
  ENDIF 

  IF keyword_set(normalize) THEN BEGIN
    ndy = n_elements(x[*,0])
    nyr = n_elements(x[0,*])
  ENDIF 

  goodx = (x[0:nel-1])
  goody = (y[0:nel-1])
  
  IF n_elements(lags) EQ 0 THEN lags = [0,2^findgen(long(alog(nel/2l-1l)/alog(2))+1l)]
  IF n_elements(avg) EQ 0 THEN avg = 2^findgen(long(alog(nel/2l-1l)/alog(2))+1l)

  lcorr = make_array(n_elements(lags),n_elements(avg),/float,value=nan())
  pval = lcorr
  dof = lcorr
  cause = make_array(n_elements(avg),2,/float,value=nan())

  FOR a = 0l,n_elements(avg)-1l DO BEGIN
    av = long(avg[a])
    print,'avg ',av
    IF av EQ 1 THEN BEGIN
      sx = goodx
      sy = goody
    ENDIF ELSE BEGIN 
      sx = make_array(nel,/float,value=nan())
      sy = sx
;      sx = make_array(nel-av+1,/float,value=nan())
;      sy = sx
      FOR k = 0l,nel-av DO sx[k] = mean(goodx[k:(av+k-1)],/nan)
      FOR k = 0l,nel-av DO sy[k] = mean(goody[k:(av+k-1)],/nan)      
    ENDELSE

    IF keyword_set(normalize) THEN BEGIN
      mn = mean(reform(sx,ndy,nyr),dim=2,/nan)
      mnr = replicate_arr(mn,nyr)
      sd = stddev(reform(sx,ndy,nyr),dim=2,/nan)
      sdr = replicate_arr(sd,nyr)
      sx = (sx-mnr) / sdr
      mn2 = mean(reform(sy,ndy,nyr),dim=2,/nan)
      mnr2 = replicate_arr(mn2,nyr)
      sd2 = stddev(reform(sy,ndy,nyr),dim=2,/nan)
      sdr2 = replicate_arr(sd2,nyr)
      sy = (sy-mnr2) / sdr2
    ENDIF
 
    sx = sx[0:nel-av]
    sy = sy[0:nel-av] 
    nel_a = n_elements(sx)

    FOR l = 0l,n_elements(lags)-1l DO BEGIN
      lg = lags[l]
 ;     print,'lag ',lg
      IF (lg EQ 0 OR lg GE av) AND lg LT (nel_a-2l) THEN BEGIN 
        lsx = sx[0:nel_a-lg-1l]
        lsy = sy[lg:nel_a-1l]
        good = wherE(finite(lsx) AND finite(lsy),ngvals)
        corr = correl(lsx,lsy)
        df = edof(lsx,lsy) < ngvals
        pvalue = corr_ttest(corr=abs(corr),N=long(df))
        lcorr[l,a] = corr
        pval[l,a] = pvalue
        dof[l,a] = df
      ENDIF 
    ENDFOR

;throw in granger test here across lags?
    IF keyword_set(grange) THEN BEGIN 
      IF n_elements(glag) EQ 0 THEN glag = lags[where(lag GE 0)]
      uselags = glag[where(glag GE av)]
      gtest = granger(sx,sy,lags=uselags,sig=sig)
      IF gtest[0] NE -1 THEN BEGIN
        cause[a,0] = min(gtest)
        cause[a,1] = max(gtest)
      ENDIF 
    ENDIF 
 
  ENDFOR 
;stop
  return,lcorr

END
