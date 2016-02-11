FUNCTION hhmm_to_fjday,hhmm,s,double=double
  hm = hhmm_to_hm(hhmm)
  IF NOT keyword_set(s) THEN sec = 0.0 ELSE sec = float(s)
  IF NOT keyword_set(double) THEN retval = (float(hm[0,*])/24.0)+(float(hm[1,*])/1440.0)+(float(sec)/86400.0) ELSE retval = (double(hm[0,*])/24.0d)+(double(hm[1,*])/1440.0d)+(double(sec)/86400.0d)
  IF n_elements(retval) EQ 1 THEN return,retval[0] ELSE return,retval
END
