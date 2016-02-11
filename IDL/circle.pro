FUNCTION circle,size
  ;sin and cos from 0 to 2pi
  ring = findgen(26)/(!pi*1.25)
  x = size*cos(ring)
  y = size*sin(ring)
  return,[transpose(x),transpose(y)]
END
