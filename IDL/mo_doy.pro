FUNCTION mo_doy,doy,yr=yr
  ;Returns the month that a specific DOY is in
;  mos = [0]
;  FOR i =0l,n_elements(doy)-1l DO BEGIN
    dy = jd_to_dy(doy,yr=yr)
    mo = fix(strmid(dy,3,2,/reverse_offset))
;    mos = [mos,mo]
;  ENDFOR
;  mos = cdr(mos)
  return,mo
END
