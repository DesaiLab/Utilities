FUNCTION Anglexy,val1,val2
  magval1 = sqrt( val1[0]^2 + val1[1]^2)
  magval2 = sqrt( val2[0]^2 + val2[1]^2)
  angle = acos(((val1[0]*val2[0])  + (val1[1]*val2[1]))  / (magval1 * magval2))
  return,angle
END
