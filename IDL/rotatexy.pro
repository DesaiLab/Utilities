PRO Rotatexy,angle,x,y,xnew,ynew
  xnew = (x * cos(angle)) + (y * (-sin(angle)))
  ynew = (x * sin(angle)) + (y * cos(angle))
END
