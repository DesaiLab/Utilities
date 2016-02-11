PRO windroseplot,val,wspd,wdeg,rr=rr,mtext=mtext,valtext=valtext,zrange=zrange,badval=badval,halt=halt,ztitle=ztitle

;Produces a 2-D gridded plot of val sorted by wspd and wdeg
;Also produces a plot of wspd,wdeg histogram (frequency occurence)
;The output is in u,v space

;val is the array of values to be sorted by wind speed and direction
;wspd is the array of wind speeds
;wdeg is the array of wind direction (in degrees)
;the above three arrays should have the same length

;rr is the range for wind speeds (default is max to min windspeed)
;mtext is additional text in the title
;ztitle is the units of val
;badval is the number for missing values (Default is -999)

;halt keyword will stop the procedure after completion

;You must press enter after first plot is drawn to get next plot

;Created by: Ankur Desai 7/2002
;Modifed 8/26/2002 to be universal for any data

;contingencies
; include: tvplot.pro, vector.pro
;
;be sure that the following variables have been set:
;device,retain=1
;device,decomposed=0
;loadct,0
;!p.background=255
;!p.color=0
;!x.style=1
;!y.style=1
;!order=1

  IF NOT keyword_set(zrange) THEN zrange = [-20,20]
  IF NOT keyword_set(valtext) THEN valtext = 'NEE'
  IF NOT keyword_set(mtext) THEN mtext = ''
  IF NOT keyword_set(ztitle) THEN ztitle = 'umol/m2 s'
  IF NOT keyword_set(badval) THEN badval = -999

  goodb = where(val NE badval AND wdeg NE badval AND wspd NE badval,numgood)

  IF numgood GT 0 THEN BEGIN 
    val = val[goodb] & wspd = wspd[goodb] & wdeg = wdeg[goodb] 
    oldval = val
    
    uv = vector_uv(wspd,wdeg)
    ux = uv[*,0]
    uy = uv[*,1]
    fixux = round(ux)
    fixuy = round(uy)
    IF NOT keyword_set(rr) THEN BEGIN 
      rr = max([abs(min(fixux,/nan)),max(fixux,/nan)])
      rr2 = max([abs(min(fixuy,/nan)),max(fixuy,/nan)])
      rr = max([rr,rr2])
    ENDIF
    rr2 = rr
    outgrid3 = make_array((rr*2)+1,(rr2*2)+1,/float,value=badval)
    outgrid4 = outgrid3 - badval
    
    FOR x = rr*(-1),rr DO BEGIN
      FOR y = rr2*(-1),rr2 DO BEGIN
        gridx = x-(rr*(-1))
        gridy = y-(rr2*(-1))
        r = where((fixux EQ x) AND (fixuy EQ y),numr)
        IF numr GE 1 THEN BEGIN
          outgrid3[gridx,gridy] = mean(val[r],/nan)
          outgrid4[gridx,gridy] = numr
        ENDIF
      ENDFOR
    ENDFOR
    
    bb = where(finite(outgrid3,/nan),numbb)
    IF numbb GT 0 THEN outgrid3[bb] = badval
    
    outgrid4=outgrid4/total(outgrid4,/nan)*100.0
    nn = where(finite(outgrid4,/nan),numnn)
    IF numnn GT 0 THEN outgrid4[nn] = 0.0

    tvplot,outgrid3,title=valtext+' as f(Ux,Uy) for '+mtext,$
      xtitle='Ux (m/s)',ytitle='Uy (m/s)',$
      bartitle=ztitle,/creverse,zrange=zrange,xrange=[rr*(-1),rr],xticks=rr*2,$
      yrange=[rr2*(-1),rr2],yticks=rr2*2,/order,barcharsize=0.66,charsize=0.5
    oplot,[0,0],[rr2*(-1),rr2]
    oplot,[rr*(-1),rr],[0,0]
    
    s = ''
    read,s
    
    tvplot,outgrid4,title='Wind histogram for '+mtext,$
      xtitle='Ux (m/s)',ytitle='Uy (m/s)',$
      bartitle='% occurence',/creverse,zrange=[round(min(outgrid4)),round(max(outgrid4))],xrange=[rr*(-1),rr],xticks=(rr*2),$
      yrange=[rr2*(-1),rr2],yticks=(rr2*2),/order,barcharsize=0.66,charsize=0.5
    oplot,[0,0],[rr2*(-1),rr2]
    oplot,[rr*(-1),rr],[0,0]
    
    ENDIF ELSE print,'No good data'

  IF keyword_set(halt) THEN stop
END
