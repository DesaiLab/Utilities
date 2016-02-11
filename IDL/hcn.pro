PRO hcn
  fname = '~/Desktop/state47_WI.txt'
  openr,fl,fname,/get_lun
  precip = make_array(366,5,/float,value=nan())
  WHILE ~eof(fl) DO BEGIN
    s = ''
    readf,fl,s
    station = long(strmid(s,0,6))
    year = long(strmid(s,6,4))
    month = long(Strmid(s,10,2))
    type = strmid(S,12,4)
    IF station EQ 475516 AND type EQ 'PRCP' AND year GE 2008 THEN BEGIN
      print,'Found data for ',year,' ',month
      s = strmid(s,16)
      ss = strsplit(s,' -',/extract)
      p = ss[0:*:2]
      dim = days_in_month(month,y=year)
      p = float(p[0:dim-1])
      loc = (dy_to_jd(string(year,format='(i4.4)')+string(month,format='(i2.2)')+'01')-1)
      print,'copying to ',loc
      precip[loc:(loc+dim-1),year-2008] = p
    ENDIF
  ENDWHILE 
  free_lun,fl
  precip[wherE(precip EQ 9999)]=nan()
  precip/=10.0*2.54
  
  stop




END
