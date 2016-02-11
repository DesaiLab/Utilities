FUNCTION current_time,utc=utc,notime=notime,nodate=nodate
;returns date and time in yyyymmddhhmm
  a = systime(/julian,utc=utc)
  caldat,a,mo,d,y,h,mn,s
  retstring = string(y,mo,d,h,mn,format='(i4.4,4i2.2)')
  IF keyword_set(notime) THEN retstring = strmid(retstring,0,8) ELSE BEGIN
    IF keyword_set(nodate) THEN retstring = str_right(retstring,4)
  ENDELSE
  return,retstring
END
