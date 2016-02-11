FUNCTION clip_array,arr,locs=locs,badval=badval
  IF keyword_set(badval) THEN arrrange = where(finite(arr) AND (arr NE badval),numrange) ELSE arrrange = where(finite(arr),numrange)
  IF numrange GT 0 THEN BEGIN 
    startloc = arrrange[0]
    endloc = arrrange[numrange-1]
    locs = [startloc,endloc]
    return,arr[startloc:endloc]
  ENDIF ELSE BEGIN
    locs = [0,0]
    return,[!values.f_nan]
  ENDELSE
END

FUNCTION ensemble_fill,inarr,hles,ptsperday=ptsperday,mindays=mindays,maxdays=maxdays,enoughdays=enoughdays,filled=filled,dev=dev,badval=badval,dodev=dodev,circular=circular

;Ensemble_fill.pro by Ankur Desai
;Modified April 11, 2003

;Dirunal ensemble filling routine for arbirtrary 1D array
;a different kind of zapbadval for time series having diurnal variation
;take a standard 1d array (arr) with ptsperday locations 
;and do ensemble diurnal average (need a minimum of enoughdays)
;at location noted in holes (or all badvals if holes is not set)

;Input:
;Arr - Vector of values - bad values are either nan OR badval
;hles - optional input - where data is bad - otherwise it is computed
;ptsperday - interval size (48 = 30 minute data) or (24 = 1 hour data)
;mindays - Minimum +/- search radius - default is +/- 7
;maxdays - Maximum +/- search radius - default is +/- 30
;enoughdays - Minimum number of points to average - default is 10
;badval - Bad value - default is -999
;dodev - Compute standard deviations too

;Ouput:
;Function returns a filled Array
;filled = flag for filled values (= number of days used to fill, -1
;  for good value, 0 for not enough good values)
;dev = standard deviation of mean for filled output (only run if dodev
;  is set)

  nan = !values.f_nan
  IF n_elements(badval) EQ 0 THEN badval = -999
  IF NOT keyword_set(inarr) THEN return,[badval] ELSE BEGIN 
    arr = inarr
    IF keyword_set(circular) THEN arr = [arr[*],arr[*],arr[*]]
    IF NOT keyword_set(hles) THEN IF finite(badval) THEN hles = where((arr EQ badval) OR (finite(arr,/nan)) OR (finite(arr,/infinity))) ELSE hles = where((finite(arr,/nan)) OR (finite(arr,/infinity)))
    IF hles[0] NE -1 THEN BEGIN 
      IF NOT keyword_set(ptsperday) THEN ptsperday = 48 ;30 minute interval
      IF NOT keyword_set(maxdays) THEN maxdays = 30 ;maximum search +/- 30 days
      IF NOT keyword_set(enoughdays) THEN enoughdays = 10 ;least number pts to avg
      IF NOT keyword_set(mindays) THEN mindays = 7 ;initially search +/- 7 days
      mindays = mindays > (enoughdays/2)
      outarr = arr
      arrsize = n_elements(arr)
      arrsizemin1 = arrsize-1
      filled = make_array(arrsize,/integer,value=-1)
      IF keyword_set(dodev) THEN dev = make_array(arrsize,/float,value=badval)
      dummy = clip_array(arr,locs=range,badval=badval)
      minloc = (range[0]-(maxdays*ptsperday)) > 0
      maxloc = (range[1]+(maxdays*ptsperday)) < (arrsize-1)
      goodhles = where((hles GE minloc) AND (hles LE maxloc),ngh,complement=badhles,ncomplement=nbh)
      IF nbh GT 0 THEN filled[hles[badhles]] = 0
      IF ngh GT 0 THEN BEGIN 
        holes = hles[goodhles]
        nholes = n_elements(holes)
        FOR i = 0,nholes-1 DO BEGIN
          loc = holes[i]
          enoughdaysflag = 0b
          ndays = mindays
          WHILE (NOT enoughdaysflag) AND (ndays LT maxdays) DO BEGIN 
            fillvals = arr[((loc-(ptsperday*ndays)) > 0):((loc+(ptsperday*ndays)) < arrsizemin1):ptsperday]
            IF finite(badval) THEN ger2 = where(finite(fillvals) AND (fillvals NE badval),nger2) ELSE ger2 = where(finite(fillvals),nger2)
            IF nger2 GE enoughdays THEN enoughdaysflag = 1b ELSE ndays = ndays+1
          ENDWHILE
          IF enoughdaysflag THEN BEGIN 
            outarr[loc] = mean(fillvals[ger2],/nan) 
            IF keyword_set(dodev) THEN dev[loc] = stddev(fillvals[ger2],/nan)
          ENDIF ELSE outarr[loc] = nan
          IF finite(outarr[loc]) AND keyword_set(nger2) THEN filled[loc] = nger2 ELSE filled[loc] = 0
        ENDFOR
      ENDIF
      IF keyword_set(circular) THEN outarr = outarr[n_elements(inarr):(2*n_elements(inarr))-1]
      return,outarr
    ENDIF ELSE return,inarr
  ENDELSE
END
