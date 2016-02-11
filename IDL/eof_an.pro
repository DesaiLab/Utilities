FUNCTION eof_an,m2,pc=pc,per_var=per_var,means=means,regrid=regrid
;compute empirical ortho func from nxm matrix
;cols are each obs point
;rows are each time point

;regrid it is a x,y,(t or z) array
  IF keyword_set(regrid) THEN BEGIN
    s2 = size(m2,/dim)
    m = reform(m2,s2[0]*s2[1],s2[2])
  ENDIF ELSE m = m2

  s = size(m,/dim)
  ncol = s[0]
  nrow = s[1]

;subtract means on request
  IF keyword_set(means) THEN BEGIN
    mns = total(m, 2)/nrow
    m-=rebin(mns,ncol,nrow)
  ENDIF 

;SVD
  matrix = (1./nrow) * (double(m) ## transpose(m))
  la_svd, matrix, w, u, v
  
;EOF analysis
  eof_an = fltarr(s[1],s[0])
  FOR j = 0,s[1]-1 DO BEGIN
    t = transpose(m) ## U[j,*]
    eof_an[j,*] = t / sqrt(total(t^2))
  ENDFOR 

;Principle components
  IF arg_present(pc) THEN BEGIN 
    pc = fltarr(s[1],s[1])
    FOR j = 0,s[1]-1 DO pc[j,*] = m ## eof_an[j,*]
  ENDIF 

;Percent variance
  IF arg_present(per_var) THEN BEGIN 
    per_var = W / Total(W) * 100.0
  ENDIF 
;stop
;regrid eof
  IF keyword_set(regrid) THEN BEGIN
    eof_an = reform(transpose(eof_an),s2[0],s2[1],s2[2])
  ENDIF 

  return,eof_an
END
