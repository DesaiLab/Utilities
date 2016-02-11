FUNCTION square,size
  s2 = size/2.0
  x = [(-1.0)*s2,s2,s2,(-1.0)*s2,(-1.0)*s2]
  y = [s2,s2,(-1.0)*s2,(-1.0)*s2,s2]
  return,[transpose(x),transpose(y)]
END
