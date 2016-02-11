FUNCTION bin_avg,obindata,odata,binsize=binsize,min=min,max=max,nbins=nbins,locations=locations,median=median,std=std,omin=omin,omax=omax,nvals=nvals,hist=hist,reverse_indices=reverse_indices,se=se,badval=badval,mn=mn,mx=mx,q25=q25,q75=q75

;average values in data according to sets of bins
;keyword inputs are identical to IDL HISTOGRAM function

;INPUT

;bindata - time series of data you want to bin by
;data - the time series you want binned
;badval - missing data value - default is nan

;binsize - size of each bin
;nbins - number of bins
;max - max bin you want to use
;min - min bin you want to use

;OUTPUT

;return value - mean of data for each bin
;nvals - number of finite values in each bin
;median - median of data for each bin
;std - standard deviation of data for each bin
;se - standard error of data for each bin
;mn - min in each bin
;mx - max in each bin

;locations - minimum bindata value for each bin
;omax - max bin
;omin - min bin
;hist - histogram output
;reverse_indices - histogram function output

;bindata and data should have the same length

  IF n_elements(odata) EQ 0 THEN odata = obindata
  IF n_elements(badval) EQ 0 THEN badval = !values.f_nan
  gv = where(finite(obindata) AND (obindata NE badval),ngv)
  IF (ngv GT 0) AND (n_elements(obindata) EQ n_elements(odata)) THEN BEGIN 
    bindata = obindata[gv]
    data = odata[gv]
    sbindata = sort(bindata)
    bindata = bindata[sbindata]
    data = data[sbindata]
    hist = histogram(bindata,binsize=binsize,nbins=nbins,max=max,min=min,reverse_indices=ri,locations=locations,omin=omin,omax=omax)
    reverse_indices=ri
    mns = make_array(n_elements(hist),value=badval,/float)
    IF arg_present(median) THEN median = mns
    IF arg_present(std) OR arg_present(se) THEN std = mns
    IF arg_present(mx) THEN mx=mns
    IF arg_present(mn) THEN mn=mns
    IF arg_present(q25) THEN q25=mns
    IF arg_present(q75) THEN q75=mns
    IF arg_present(nvals) OR arg_present(se) THEN nvals = make_array(n_elements(hist),value=0,/long)
    IF arg_present(se) THEN se = mns
    FOR i = 0,n_elements(hist)-1 DO BEGIN
      IF ri[i+1] NE ri[i] THEN BEGIN
        thedata = data[ri[ri[i]:ri[i+1]-1]]
        gv2 = where(finite(thedata) AND (thedata NE badval),ngv2)
        IF ngv2 GT 0 THEN BEGIN 
          mns[i] = mean(thedata[gv2])
          IF arg_present(median) THEN median[i] = median(thedata[gv2])
        ENDIF 
        IF (arg_present(std) OR arg_present(se)) AND (ngv2 GT 1) THEN std[i] = stddev(thedata[gv2])
        IF arg_present(nvals) OR arg_present(se) THEN nvals[i] = ngv2
        IF arg_present(mn) THEN mn[i] = min(thedata[gv2])
        IF arg_present(mx) THEN mx[i] = max(thedata[gv2])
        IF arg_present(q25) THEN BEGIN
;          gv3 = where(thedata[gv2] LE median(thedata[gv2]),ngv3)
;          IF ngv3 GT 0 THEN q25[i] = median((thedata[gv2])[gv3])
          gv3 = (thedata[gv2])[sort(thedata[gv2])]
          yel = n_elements(gv3)
          IF yel GT 0 THEN BEGIN 
            IF (yel MOD 2) NE 0 THEN fqloc = ((yel+1)/4)-1 ELSE fqloc = ((yel+2)/4)-1
            q25[i] = gv3[fqloc]
          ENDIF 
        ENDIF 
        IF arg_present(q75) THEN BEGIN
;          gv3 = where(thedata[gv2] GE median(thedata[gv2]),ngv3)
;          IF ngv3 GT 0 THEN q75[i] = median((thedata[gv2])[gv3])
          gv3 = (thedata[gv2])[sort(thedata[gv2])]
          yel = n_elements(gv3)
          IF yel GT 0 THEN BEGIN 
            IF (yel MOD 2) NE 0 THEN tqloc = (((3*yel)+3)/4)-1 ELSE tqloc = (((3*yel)+2)/4)-1
            q75[i] = gv3[tqloc]
          ENDIF 
        ENDIF
      ENDIF  
    ENDFOR 
    IF arg_present(se) THEN BEGIN
      gv3 = where((nvals NE 0) AND (nvals NE badval) AND finite(nvals),ngv3)
      IF ngv3 GT 0 THEN se[gv3] = std[gv3] / sqrt(nvals[gv3])
    ENDIF 
  ENDIF ELSE mns = badval
  return,mns
END
