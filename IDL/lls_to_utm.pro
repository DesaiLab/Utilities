PRO LLs_to_UTM,lats,lons,easts,norths

i = min([n_elements(lats),n_elements(lons)])
easts = dblarr(i)
norths = dblarr(i)

FOR x = 0,i-1 DO BEGIN
  en = ll_to_utm(lats[x],lons[x])
  easts[x] = en[0]
  norths[x] = en[1]
ENDFOR

END
