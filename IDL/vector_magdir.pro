FUNCTION Vector_MagDir, u, v
;takes two orthogonal vectors, u and v and returns bearing from v, and magnitude

u_pos = abs(u)
v_pos = abs(v)
u_neg = u lt 0.0
v_neg = v lt 0.0

mag = sqrt( (u_pos^2) + (v_pos^2))

dir = acos ( v_pos / mag )
dir = rad_to_deg(dir)

;get the quadrant information
;true = 1, false = 0
quad = (4 * u_neg) + (2 * v_neg)

;u_pos v_pos - do nothing
;u_pos v_neg   180-dir
;u_neg v_neg
;u_neg v_pos

;quad_add = (0.0 * (quad eq 0)) + (90.0 * (quad eq 2)) + (180.0 *
;(quad eq 6)) + (270.0 * (quad eq 4))

quad_add = (0.0 * (quad eq 0)) + (-180.0 * (quad eq 2)) + (180.0 * (quad eq 6)) + (-360.0 * (quad eq 4))

dir = abs(dir + quad_add)

out_var = fltarr(n_elements(mag[*,0]),n_elements(mag[0,*]),2)

out_var[*,*,0] = mag
out_var[*,*,1] = dir
return, reform(out_var)
END

FUNCTION Vector_MagDir2, u, v,r=r,a=a
;problem with vector_magdir when v = 0, use arctan instead

x = double(u)
y = double(v)

w = where((x NE 0) OR (y NE 0),count)
a = u*0
IF count GT 0 THEN a[w]=atan(x[w],y[w])
w = where(a LT 0,count)
IF count GT 0 THEN a[w]=a[w]+2*!dpi
r = sqrt(x^2+y^2)
a = rad_to_deg(a)
return,transpose([float(r),float(a)])
END
