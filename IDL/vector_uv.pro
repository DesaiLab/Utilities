FUNCTION Vector_UV, mag, dir

;reverse of above function   vector_uv(vector_magdir(m)) = m
;much simpler

dirrad = deg_to_rad(dir)

u = mag * sin(dirrad)
v = mag * cos(dirrad)

if n_elements(mag) eq 1 then out_var = [u,v] else begin
  out_var = fltarr(n_elements(u[*,0]),n_elements(u[0,*]),2)
  out_var[*,*,0] = u
  out_var[*,*,1] = v
endelse

return, reform(out_var)
END
