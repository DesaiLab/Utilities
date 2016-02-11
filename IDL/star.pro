FUNCTION star,size
  s2 = size/2.0
  y1 = sin(54. * !pi/180.)
  y2 = s2 * sin(18. * (!pi/180.))
  delta2 = y2/y1
  del = replicate_arr([s2,delta2],5)
  i = findgen(10)
  theta = (90. + (36. * i)) * (!pi/180.)
  x = del * cos(theta)
  y = del * sin(theta)
  x = [x,x[0]]
  y = [y,y[0]]
  return,[transpose(x),transpose(y)]
END
