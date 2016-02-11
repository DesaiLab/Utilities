FUNCTION mergevars,t1,t2,model=model,length_OF_day=length_OF_day,badval=badval,shortgap=shortgap
;Given two time series, t1 and t2
;t1 is the high rate time series with gaps
;t2 is the averaged time series without gaps
;length_of_day is number of point in t1 that constitute one 24-hour
;day (default is 24 - i.e., t1 is hourly)  
;gaps in t1 are indicated with NAN or badval (if set)
;shortgap is the largest gaps to be filled with linear interpolation
;(default is 3 time points in t1)

;Function debiases and downscales t2 to match features of t1 and fills
;gaps in t1 

;assume time series in T2 is an average of X T1 points starting at
;beginning of time that is in first set of each point for T1

;output is a gap-filled time series of t1
;second output, model, is the model used to fit t2 to t1, from which
;statistics (RMSE, r^2, bias) could be computed

;Based partly on Dan Ricciuto's NACP met filling (which doesn't do
;most of this except for temperature)


  IF n_elements(badval) NE 0 THEN gaps = where(~finite(t1) OR t1 EQ badval,ngaps) ELSE gaps = where(~finite(t1),ngaps)

;Step 0 - if no gaps in t1, go home, else convert badvals to NAN
  IF ngaps EQ 0 THEN BEGIN
    print,'No gaps!'
    model = t1
    return,t1
  ENDIF 

  t1_nan = t1
  t1_nan[gaps] = !values.f_nan

;Step 1 - check inputs - is number of elements in T1 a multiple of T2?
  n_t1 = n_elements(t1_nan)
  n_t2 = n_elements(t2)
  IF double(long(n_t1)/long(n_t2)) NE (double(n_t1)/double(n_t2)) THEN BEGIN
    print,'Number of elements in T1 is not a multiple of T2 ',n_t1,n_t2
    stop
  ENDIF 

;step 2 - if t1 and t2 are on same time freq, skip step 3
  IF n_t1 NE n_t2 THEN BEGIN 

;Step 3 - fill short gaps in T1 with linear interpolation
    IF n_elements(shortgap) EQ 0 THEN shortgap = 3
    
    IF shortgap NE 0 THEN BEGIN 
;go to each gap, if value before finite = beginning of gap, if a
;goodvalue is found in next 1-shortgap then we can interpolate 
      FOR i = 0,ngaps-1 DO BEGIN
        cur_gap = gaps[i]
        IF (cur_gap NE 0) AND (cur_gap NE n_t1-1) AND (~finite(t1_nan[cur_gap])) THEN BEGIN    ;make sure not at beginning or end of time series or previously filled
          IF finite(t1_nan[cur_gap-1]) THEN BEGIN                                              ;if value before gap is finite then we are at start of gap
            nextgval = where(finite(t1_nan[cur_gap:((cur_gap+shortgap) < (n_t1-1))]),n_nextgval) ;find next good value, watch for end of timeseries
            IF n_nextgval NE 0 THEN BEGIN                                                        ;we found a good value - this is a short gap! 
            ;interpolate between cur_gap-1 to cur_gap+nextgval
              interpolates = interpol([t1_nan[cur_gap-1],t1_nan[cur_gap+nextgval[0]]],[cur_gap-1,cur_gap+nextgval[0]],cur_gap+findgen(nextgval[0]))
              t1_nan[cur_gap:cur_gap+nextgval[0]-1] = interpolates
            ENDIF 
          ENDIF 
        ENDIF
      ENDFOR 
    ENDIF
  ENDIF 
  biggaps = where(~finite(t1_nan),n_biggaps)  

  IF n_biggaps EQ 0 THEN BEGIN
    print,'No big or edge gaps'
    return,t1_nan
  ENDIF ELSE BEGIN 

;Step 4 - De-bias T2 with either Q-Q regression or normaliziation,
;         currently the latter
    t2_norm = (t2-mean(t2,/nan))/stddev(t2,/nan)
    t1_mean = mean(t1_nan,/nan)
    t1_stddev = stddev(t1_nan,/nan)
    t2_debias = (t2_norm*t1_stddev)+t1_mean
    t1_norm = (t1_nan-mean(t1_nan,/nan))/stddev(t1_nan,/nan)

;Step 5 - if t1 and t2 are not same timescale, then time to fix that
    IF n_t1 EQ n_t2 THEN BEGIN
      t2_debias_downscale = t1_nan
    ENDIF ELSE BEGIN
;advanced method - map shape of t1 to t2 - for now, just linear
;                  interpolation
;but if we're using daily values

;linear version
      t2_debias_downscale = interpol(t2_debias,findgen(n_t2)*(n_t1/n_t2),findgen(n_t1))
    ENDELSE 

;notes if only 1 values, match shape 


;if precip - have to distribute and divde for total


;Step 6 - swap in modeled values for new values
    model = t2_debias_downscale
    t1_nan[biggaps] = t2_debias_downscale[biggaps]
    return,t1_nan
  ENDELSE 

END
