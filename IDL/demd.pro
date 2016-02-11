PRO startsandends,d,starts=starts,ends=ends
  data = [reform(d),0]
  loc = ~finite(data)
  ud = uniq(loc)
  wloc = where(loc,nw)

  IF nw EQ 0 THEN BEGIN
    starts = -1
    ends = -1
  ENDIF ELSE BEGIN 
    goode = in(wloc,ud)
    ends = ud[where(goode)]

    rloc = reverse(loc)
    udd = reverse(n_elements(loc)-uniq(rloc)-1)
    goods = in(wloc,udd)
    starts = udd[where(goods)]
  ENDELSE 

END

FUNCTION demd,data,SHIFTFACTOR=shiftfactor,QUEK=quekopt, SPLINEMEAN=splinemeanopt, ZEROCROSS=zerocrossopt,VERBOSE=verboseopt,nodel=nodel
  startsandends,data,starts=starts,ends=ends
  IF starts[0] NE -1 THEN BEGIN
    n = n_elements(data)
    k = 0l
    md = data
    gapsize = (ends-starts)+1
    FOR i = 0,n_elements(starts)-1 DO BEGIN
      half = gapsize[i]/2.0
      half_s = long(half)
      half_e = round(half)   
      s = starts[i]
      e = ends[i]
      IF half_s GT 0 THEN BEGIN 
        firsthalf = (numgen(s-half_s,s-1))>0
        md[s:s+half_s-1] = reverse(md[firsthalf])
      ENDIF 
      secondhalf = (numgen(e+1,e+half_e))<(n-1)
      md[s+half_s:e] = reverse(md[secondhalf])
    ENDFOR 
  ENDIF ELSE md = data

  IF ~finite(md[0]) THEN md[0] = md[1]

  imf=emd(zapbadval(md),SHIFTFACTOR=shiftfactor,QUEK=quekopt, SPLINEMEAN=splinemeanopt, ZEROCROSS=zerocrossopt,VERBOSE=verboseopt)

  IF ~keyword_set(nodel) THEN BEGIN 
    bval = where(~finite(data),nbv) 
    IF nbv GT 0 THEN FOR k = 0,n_elements(imf[0,*])-1 DO imf[bval,k]=!values.f_nan
  ENDIF 

  return,imf

END
