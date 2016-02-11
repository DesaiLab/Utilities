FUNCTION hm_to_hhmm,h,m
  hh = string(h,format='(i2.2)')
  mm = string(m,format='(i2.2)')
  return,hh+mm
END
