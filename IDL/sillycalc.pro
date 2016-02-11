PRO sillycalc

  l = numgen(1.0,82.0,1.0)
  w = l
  h = l

  testarr = make_array(5,n_elements(l)^3,/double)
  testarr[0,*] = expand_arr(l,n_elements(w)*n_elements(h))
  testarr[1,*] = replicate_arr(expand_arr(w,n_elements(h)),n_elements(l))
  testarr[2,*] = replicate_arr(h,n_elements(l)*n_elements(w))
  testarr[3,*] = testarr[0,*] + (2*testarr[1,*]) + (2*testarr[2,*])
  testarr[4,*] = (testarr[0,*] * testarr[1,*] * testarr[2,*])/1728.
  oversize = where(testarr[3,*] GT 82.,nos)
  IF nos GT 0 THEN testarr[4,oversize] = nan()
  overlen = where(testarr[0,*] LT testarr[1,*],nol)
  IF nol GT 0 THEN testarr[4,oversize] = nan()
  overlen2 = where(Testarr[0,*] LT testarr[2,*],nol2)
  IF nol2 GT 0 THEN testarr[4,oversize] = nan()
  FOR step = 1,48 DO BEGIN
    lloc = where(Testarr[0,*] EQ step)
    maxv = where(testarr[4,lloc] EQ max(testarr[4,lloc]))
    print,testarr[*,lloc[maxv[0]]],format='(5f10.2)'
  ENDFOR 


  stop


END
