PRO array_slope,xin,y,slope=b,intercept=a,correlation=c
;takes an [x,y,z] array and returns [x,y] slope, [x,y] intercept,
;[x,y] correlation
  ss = n_elements(xin)
  x = y
  FOR i = 0,ss-1 DO x[*,*,i] = xin[i]
  sx = total(xin)
  sy = TOTAL(y,3)
  t = x - sx/ss
  b = TOTAL(t * y,3)
  st2 = TOTAL(t^2, 3)
  b = b / st2
  a = (sy - sx * b) / ss
  ydev = y
  sy/=ss
  FOR i = 0,ss-1 DO ydev[*,*,i] -= sy
  dx2 = total(abs(t)^2,3)
  dy2 = total(abs(ydev)^2,3)
  c = total(t * ydev,3)/(sqrt(dx2)*sqrt(dy2))
END
