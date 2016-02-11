PRO lineslope,pts,m,b

;pts are [x1,y1,x2,y2]
;m is slope
;b is intercept

m = (pts[3]-pts[1])/(pts[2]-pts[0])
b = pts[1] - (m * pts[0])

END
