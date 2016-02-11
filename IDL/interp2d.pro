FUNCTION Interp2D,srf,xrange,yrange,x,y

;give an xrange and yrange for a surface, return value for x,y

  nx = [0,n_elements(srf[*,0])-1]
  ny = [0,n_elements(srf[0,*])-1]

  lineslope,float([xrange[0],nx[0],xrange[1],nx[1]]),mx,bx
  lineslope,float([yrange[0],ny[0],yrange[1],ny[1]]),my,by

  xloc = bx + (mx * x)
  yloc = by + (my * y)

  return,interpolate(srf,xloc,yloc)
END
