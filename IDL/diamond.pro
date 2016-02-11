FUNCTION diamond,size
  s2 = size/2.0
  x = [0,s2,0,(-1.0)*s2,0]
  y = [s2,0,(-1.0)*s2,0,s2]
  return,[transpose(x),transpose(y)]
END
