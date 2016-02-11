FUNCTION triangle,size
  x = [0.,0.5*size,(-0.5)*size,0]
  r3 = sqrt(3)/4.
  y = [r3*size,r3*(-1.)*size,r3*(-1.)*size,r3*size]
  return,[transpose(x),transpose(y)]
END
